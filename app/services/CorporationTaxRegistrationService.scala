/*
 * Copyright 2024 HM Revenue & Customs
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

import cats.implicits._
import connectors.{BusinessRegistrationConnector, IncorporationCheckAPIConnector, IncorporationInformationConnector}
import helpers.DateHelper
import jobs.{LockResponse, MongoLocked, ScheduledService, UnlockingFailed}
import models.des.BusinessAddress
import models.validation.APIValidation._
import models.{HttpResponse => _, _}
import repositories.{CorporationTaxRegistrationMongoRepository, Repositories, SequenceMongoRepository}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.lock.LockService
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import utils.{Logging, StringNormaliser}

import java.time.Instant
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NoStackTrace
import scala.util.{Success, Try}

@Singleton
class CorporationTaxRegistrationServiceImpl @Inject()(val submissionCheckAPIConnector: IncorporationCheckAPIConnector,
                                                      val brConnector: BusinessRegistrationConnector,
                                                      val submissionEventService: SubmissionEventService,
                                                      val incorpInfoConnector: IncorporationInformationConnector,
                                                      val repositories: Repositories,
                                                      val auditConnector: AuditConnector,
                                                      servicesConfig: ServicesConfig
                                                     )(implicit val ec: ExecutionContext) extends CorporationTaxRegistrationService {

  lazy val cTRegistrationRepository: CorporationTaxRegistrationMongoRepository = repositories.cTRepository
  lazy val sequenceRepository: SequenceMongoRepository = repositories.sequenceRepository

  lazy val lockoutTimeout: Int = servicesConfig.getInt("schedules.missing-incorporation-job.lockTimeout")

  def instantNow: Instant = Instant.now()

  lazy val lockKeeper: LockService =
    LockService(repositories.lockRepository, "missing-incorporation-job-lock", lockoutTimeout.seconds)
}

sealed trait FailedPartialForLockedTopup extends NoStackTrace

case object NoSessionIdentifiersInDocument extends FailedPartialForLockedTopup

trait CorporationTaxRegistrationService extends ScheduledService[Either[String, LockResponse]] with DateHelper with Logging  {

  implicit val ec: ExecutionContext
  val cTRegistrationRepository: CorporationTaxRegistrationMongoRepository
  val sequenceRepository: SequenceMongoRepository
  val brConnector: BusinessRegistrationConnector
  val auditConnector: AuditConnector
  val incorpInfoConnector: IncorporationInformationConnector
  val submissionEventService: SubmissionEventService
  val submissionCheckAPIConnector: IncorporationCheckAPIConnector
  val lockKeeper: LockService

  def instantNow: Instant

  def updateRegistrationProgress(regID: String, progress: String): Future[Option[String]] = cTRegistrationRepository.updateRegistrationProgress(regID, progress)


  def createCorporationTaxRegistrationRecord(internalId: String, registrationId: String, language: String): Future[CorporationTaxRegistration] = {
    val record = CorporationTaxRegistration(
      internalId = internalId,
      registrationID = registrationId,
      formCreationTimestamp = formatTimestamp(instantNow),
      language = language)

    cTRegistrationRepository.createCorporationTaxRegistration(record)
  }

  def retrieveCorporationTaxRegistrationRecord(rID: String, lastSignedIn: Option[Instant] = None): Future[Option[CorporationTaxRegistration]] = {
    val repo = cTRegistrationRepository
    for {
      doc <- repo.findOneBySelector(repo.regIDSelector(rID))
      _ <- lastSignedIn.traverse(date => repo.updateLastSignedIn(rID, date))
    } yield doc
  }

  private def returnSeqAddressLinesFromCHROAddress(rOAddress: CHROAddress): Seq[Option[String]] = {
    val line1Result = Some(rOAddress.premises + " " + rOAddress.address_line_1)
    val line2Result = Some(rOAddress.address_line_2.getOrElse(rOAddress.locality))
    val line3Result = if (rOAddress.address_line_2.isDefined) Some(rOAddress.locality) else rOAddress.region
    val line4Result = rOAddress.address_line_2 flatMap (_ => rOAddress.region)

    List(line1Result, line2Result, line3Result, line4Result)
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
  }

  private def returnOptPostcode(rOAddress: CHROAddress): Option[String] = {
    val postCodeOpt: Option[String] = rOAddress.postal_code map (pc => StringNormaliser.normaliseString(pc, postCodeInvert).take(20))
    postCodeOpt foreach { postCode =>
      if (!postCode.matches(postCodePattern.regex)) throw new Exception("Post code did not match validation")
    }
    postCodeOpt
  }

  private def returnOptCountry(rOAddress: CHROAddress): Option[String] = {
    val countryOpt = Some(StringNormaliser.normaliseString(rOAddress.country, countryInvert).take(20))

    countryOpt foreach { country =>
      if (!country.matches(countryPattern.regex)) throw new Exception("Country did not match validation")
    }
    countryOpt
  }

  def convertRoToBusinessAddress(rOAddress: CHROAddress): Option[BusinessAddress] =
    Try {
      val linesResults: Seq[Option[String]] = returnSeqAddressLinesFromCHROAddress(rOAddress)
      val postCodeOpt: Option[String] = returnOptPostcode(rOAddress)
      val countryOpt: Option[String] = returnOptCountry(rOAddress)

      BusinessAddress(linesResults.head.get, linesResults(1).get, linesResults(2), linesResults(3), postCodeOpt, countryOpt)
    }.map { bAddress =>
      Some(bAddress)
    }.recoverWith {
      case e => logger.info(s"[convertRoToBusinessAddress] Could not convert RO address - ${e.getMessage}")
        Success(Option.empty)
    }.get

  def convertROToPPOBAddress(rOAddress: CHROAddress): Option[PPOBAddress] =
    Try {
      val linesResults: Seq[Option[String]] = returnSeqAddressLinesFromCHROAddress(rOAddress)
      val postCodeOpt: Option[String] = returnOptPostcode(rOAddress)
      val countryOpt: Option[String] = returnOptCountry(rOAddress)

      PPOBAddress(linesResults.head.get, linesResults(1).get, linesResults(2), linesResults(3), postCodeOpt, countryOpt, None, "")
    }.map { s =>
      Some(s)
    }.recoverWith {
      case e => logger.warn(s"[convertROToPPOBAddress] Could not convert RO address - ${e.getMessage}")
        Success(None)
    }.get

  def retrieveConfirmationReferences(rID: String): Future[Option[ConfirmationReferences]] =
    cTRegistrationRepository.retrieveConfirmationReferences(rID)

  def invoke(implicit ec: ExecutionContext): Future[Either[String, LockResponse]] = {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    lockKeeper.withLock(locateOldHeldSubmissions).map {
      case None => Right(MongoLocked)
      case Some(res) =>
        logger.info(s"[invoke] CorporationTaxRegistrationService acquired lock and returned results\nResult locateOldHeldSubmissions: $res")
        Left(res)
    }.recover {
      case e: Exception =>
        logger.error(s"[invoke] Error running locateOldHeldSubmissions with message: ${e.getMessage}")
        Right(UnlockingFailed)
    }
  }

  def locateOldHeldSubmissions(implicit hc: HeaderCarrier): Future[String] = {
    cTRegistrationRepository.retrieveAllWeekOldHeldSubmissions().map { submissions =>
      if (submissions.nonEmpty) {
        logger.error("ALERT_missing_incorporations")
        submissions.map { submission =>
          val txID = submission.confirmationReferences.fold("")(cr => cr.transactionId)
          val heldTimestamp = submission.heldTimestamp.fold("")(ht => ht.toString)

          logger.warn(s"[locateOldHeldSubmissions] Held submission older than one week of regID: ${submission.registrationID} txID: $txID heldDate: $heldTimestamp)")
          true
        }
        "Week old held submissions found"
      } else {
        "No week old held submissions found"
      }
    }
  }


  final class FailedToGetCTData extends NoStackTrace

}
