/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package services

import config.MicroserviceAuditConnector
import connectors._
import helpers.DateHelper
import javax.inject.Inject
import models.validation.APIValidation
import models.{HttpResponse => _, _}
import org.joda.time.{DateTime, DateTimeZone}
import play.api.Logger
import repositories._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import utils.StringNormaliser

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}

class CorporationTaxRegistrationServiceImpl @Inject()(
                                                       val submissionCheckAPIConnector: IncorporationCheckAPIConnector,
                                                       val brConnector: BusinessRegistrationConnector,
                                                       val desConnector: DesConnector,
                                                       val incorpInfoConnector: IncorporationInformationConnector,
                                                       val repositories: Repositories
                                                     ) extends CorporationTaxRegistrationService {

  val cTRegistrationRepository: CorporationTaxRegistrationMongoRepository = repositories.cTRepository
  val sequenceRepository: SequenceMongoRepository = repositories.sequenceRepository
  val stateDataRepository: StateDataMongoRepository = repositories.stateDataRepository

  lazy val auditConnector = MicroserviceAuditConnector

  def currentDateTime: DateTime = DateTime.now(DateTimeZone.UTC)
}

sealed trait FailedPartialForLockedTopup extends NoStackTrace
case object NoSessionIdentifiersInDocument extends FailedPartialForLockedTopup

trait CorporationTaxRegistrationService extends DateHelper {

  val cTRegistrationRepository: CorporationTaxRegistrationRepository
  val sequenceRepository: SequenceRepository
  val stateDataRepository: StateDataRepository
  val brConnector: BusinessRegistrationConnector
  val auditConnector: AuditConnector
  val incorpInfoConnector :IncorporationInformationConnector
  val desConnector: DesConnector
  val submissionCheckAPIConnector: IncorporationCheckAPIConnector

  def currentDateTime: DateTime

  def updateRegistrationProgress(regID: String, progress: String): Future[Option[String]] = cTRegistrationRepository.updateRegistrationProgress(regID, progress)


  def createCorporationTaxRegistrationRecord(internalId: String, registrationId: String, language: String): Future[CorporationTaxRegistration] = {
    val record = CorporationTaxRegistration(
      internalId = internalId,
      registrationID = registrationId,
      formCreationTimestamp = formatTimestamp(currentDateTime),
      language = language)

    cTRegistrationRepository.createCorporationTaxRegistration(record)
  }

  def retrieveCorporationTaxRegistrationRecord(rID: String, lastSignedIn: Option[DateTime] = None): Future[Option[CorporationTaxRegistration]] = {
    val repo = cTRegistrationRepository
    repo.retrieveCorporationTaxRegistration(rID) map {
      doc =>
        lastSignedIn map ( repo.updateLastSignedIn(rID, _))
        doc
    }
  }

  def convertROToPPOBAddress(rOAddress: CHROAddress) : Option[PPOBAddress] = {
    import APIValidation._

    Try {
      val line1Result = Some(rOAddress.premises + " " + rOAddress.address_line_1)
      val line2Result = Some(rOAddress.address_line_2.getOrElse(rOAddress.locality))
      val line3Result = if (rOAddress.address_line_2.isDefined) Some(rOAddress.locality) else rOAddress.region
      val line4Result = rOAddress.address_line_2 flatMap (_ => rOAddress.region)

      val linesResults: Seq[Option[String]] = List(line1Result, line2Result, line3Result, line4Result)
        .zipWithIndex
        .map { linePair =>
          val (optLine, index) = linePair
          optLine map { line =>
            val normalisedString = StringNormaliser.normaliseString(line, lineInvert).take(if (index == 3) 18 else 27)
            val regex = (if (index == 3) line4Pattern else linePattern).regex
            if (!normalisedString.matches(regex)) throw new Exception(s"Line $index did not match validation")
            normalisedString
          }
        }

      val postCodeOpt = rOAddress.postal_code map (pc => StringNormaliser.normaliseString(pc, postCodeInvert).take(20))
      val countryOpt = Some(StringNormaliser.normaliseString(rOAddress.country, countryInvert).take(20))

      postCodeOpt foreach { postCode =>
        if (!postCode.matches(postCodePattern.regex)) throw new Exception("Post code did not match validation")
      }
      countryOpt foreach { country =>
        if (!country.matches(countryPattern.regex)) throw new Exception("Country did not match validation")
      }

      PPOBAddress(linesResults.head.get, linesResults(1).get, linesResults(2), linesResults(3), postCodeOpt, countryOpt, None, "")
    } match {
      case Success(ppob) =>
        Logger.info(s"[convertROToPPOBAddress] Converted RO address")
        Some(ppob)
      case Failure(e)    =>
        Logger.warn(s"[convertROToPPOBAddress] Could not convert RO address - ${e.getMessage}")
        None
    }
  }

  def retrieveConfirmationReferences(rID: String): Future[Option[ConfirmationReferences]] = {
    cTRegistrationRepository.retrieveConfirmationReferences(rID)
  }

  def locateOldHeldSubmissions(implicit hc : HeaderCarrier): Future[String] = {
    cTRegistrationRepository.retrieveAllWeekOldHeldSubmissions().map {submissions =>
      if (submissions.nonEmpty) {
        Logger.error("ALERT_missing_incorporations")
        submissions.map {submission =>
          val txID = submission.confirmationReferences.fold("")(cr => cr.transactionId)
          val heldTimestamp = submission.heldTimestamp.fold("")(ht => ht.toString())

          Logger.warn(s"Held submission older than one week of regID: ${submission.registrationID} txID: $txID heldDate: $heldTimestamp)")
          true
        }
        "Week old held submissions found"
      } else {
        "No week old held submissions found"
      }
    }
  }

  final class FailedToGetCTData extends NoStackTrace

  def retrieveCTData(regId: String): Future[CorporationTaxRegistration] = {
    cTRegistrationRepository.retrieveCorporationTaxRegistration(regId) flatMap {
      case Some(ct) => Future.successful(ct)
      case _ => Future.failed(new FailedToGetCTData)
    }
  }

}
