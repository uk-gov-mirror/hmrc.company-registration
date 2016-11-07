/*
 * Copyright 2016 HM Revenue & Customs
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

import connectors._
import models._
import org.joda.time.DateTime
import play.api.Logger
import play.api.libs.json.{JsObject, Json}
import repositories._
import uk.gov.hmrc.play.http.HeaderCarrier
import play.api.http.Status._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NoStackTrace

object RegistrationHoldingPenService extends RegistrationHoldingPenService {

  override val desConnector = DesConnector
  override val stateDataRepository = Repositories.stateDataRepository
  override val ctRepository = Repositories.cTRepository
  override val incorporationCheckAPIConnector = IncorporationCheckAPIConnector
  override val heldRepo = Repositories.heldSubmissionRepository
  override val accountingService = AccountingDetailsService

}

private[services] class InvalidSubmission(val message: String) extends NoStackTrace
private[services] class MissingAckRef(val message: String) extends NoStackTrace
private[services] class UnexpectedStatus(val status: String) extends NoStackTrace

trait RegistrationHoldingPenService {

  val desConnector : DesConnector
  val stateDataRepository: StateDataRepository
  val incorporationCheckAPIConnector: IncorporationCheckAPIConnector
  val ctRepository : CorporationTaxRegistrationRepository
  val heldRepo : HeldSubmissionRepository
  val accountingService : AccountingDetailsService

  private[services] class FailedToRetrieveByTxId  extends NoStackTrace
  private[services] class FailedToRetrieveByAckRef extends NoStackTrace
  private[services] class MissingAccountingDates extends NoStackTrace

  def updateNextSubmissionByTimepoint(): Future[Seq[JsObject]] = {
    fetchIncorpUpdate flatMap { items =>
      val results = items.map { item =>
        updateSubmission(item)
      }
      Future.sequence(results)
    }
  }

  private[services] def updateSubmission(item: IncorpUpdate): Future[JsObject] = {
    fetchRegistrationByTxId(item.transactionId) flatMap { ctReg =>
      import RegistrationStatus.{HELD,SUBMITTED,DRAFT}
      ctReg.status match {
        case HELD => updateHeldSubmission(item, ctReg)
        case SUBMITTED => updateSubmittedSubmission() // TODO SCRS-2298
        case unknown => updateOtherSubmission(ctReg.registrationID, item.transactionId, unknown)
      }
    }
  }

  private[services] def updateHeldSubmission(item: IncorpUpdate, ctReg: CorporationTaxRegistration): Future[JsObject] = {
    getAckRef(ctReg) match {
      case Some(ackRef) => {
        val response = for {
          submission <- constructFullSubmission(item, ctReg, ackRef)
          response <- postSubmissionToDes(ackRef, submission)
        } yield {
          response
        }
        response flatMap {
          case SuccessDesResponse(response) => processSuccessDesResponse(item, ctReg, response)
          case InvalidDesRequest(message) => processInvalidDesRequest(ackRef, message)
          case NotFoundDesResponse => processNotFoundDesResponse(ackRef)
        }
      }
      case None => {
        val errMsg = s"""Held Registration doc is missing the ack ref for tx_id "${item.transactionId}"."""
        Logger.error(errMsg)
        Future.failed(new MissingAckRef(errMsg))
      }
    }
  }


  private def processSuccessDesResponse(item: IncorpUpdate, ctReg: CorporationTaxRegistration, response: JsObject): Future[JsObject] = {
    ctRepository.updateSubmissionForIncorp(ctReg.registrationID, item.crn, "")
    Future.successful(response)
  }

  private def processInvalidDesRequest(ackRef: String, message: String) = {
    val errMsg = s"""Invalid request sent to DES for ack ref ${ackRef} - reason "${message}"."""
    Logger.error(errMsg)
    Future.failed(new InvalidSubmission(errMsg))
  }

  private def processNotFoundDesResponse(ackRef: String) = {
    val errMsg = s"""Request sent to DES for ack ref ${ackRef} not found" """
    Logger.error(errMsg)
    Future.failed(new InvalidSubmission(errMsg))
  }


  private def updateSubmittedSubmission(): Future[JsObject] = ???  // TODO SCRS-2298

  private def updateOtherSubmission(regId: String, txId: String, status: String): Future[JsObject] = {
    Logger.error(s"""Tried to process a submission (${regId}/${txId}) with an unexpected status of "${status}" """)
    Future.failed(new UnexpectedStatus(status))
  }


  private def constructFullSubmission(item: IncorpUpdate, ctReg: CorporationTaxRegistration, ackRef: String): Future[JsObject] = {
    for {
      heldData <- fetchHeldData(ackRef)
      dates <- calculateDates(item, ctReg.accountingDetails, ctReg.accountsPreparation)
    } yield {
      appendDataToSubmission(item.crn, dates, heldData.submission)
    }
  }

  private def asDate(s: String): DateTime = {
    // TODO SCRS-2298 - find the usage of this and refactor so that the repo returns a DateTime
    DateTime.parse(s)
  }

  private[services] def formatDate(date: DateTime): String = {
    date.toString("yyyy-MM-dd")
  }

  private def getAckRef(reg: CorporationTaxRegistration): Option[String] = {
    reg.confirmationReferences map (_.acknowledgementReference )
  }

  private[services] def fetchIncorpUpdate(): Future[Seq[IncorpUpdate]] = {
    val hc = new HeaderCarrier()
    for {
      timepoint <- stateDataRepository.retrieveTimePoint
      submission <- incorporationCheckAPIConnector.checkSubmission(timepoint)(hc)
    } yield {
      submission.items //TODO SCRS-2298 This needs to return the full sequence, for now it returns the first only
    }
  }

  private[services] def fetchRegistrationByTxId(transId : String): Future[CorporationTaxRegistration] = {
    ctRepository.retrieveRegistrationByTransactionID(transId) flatMap {
      case Some(s) => Future.successful(s)
      case None => Future.failed(new FailedToRetrieveByTxId)
    }
  }

  private[services] def activeDate(date: AccountingDetails) = {
    import AccountingDetails.WHEN_REGISTERED
    (date.accountingDateStatus, date.startDateOfBusiness) match {
      case (_, Some(givenDate))  => ActiveInFuture(asDate(givenDate))
      case (status, _) if status == WHEN_REGISTERED => ActiveOnIncorporation
      case _ => DoNotIntendToTrade
    }
  }

  private[services] def fetchHeldData(ackRef: String) = {
    heldRepo.retrieveSubmissionByAckRef(ackRef) flatMap {
      case Some(held) => Future.successful(held)
      case None => Future.failed(new FailedToRetrieveByAckRef)
    }
  }

  private[services] def calculateDates(item: IncorpUpdate,
  accountingDetails: Option[AccountingDetails],
  accountsPreparation: Option[PrepareAccountMongoModel]): Future[SubmissionDates] = {

    accountingDetails map { details =>
      val prepDate = accountsPreparation flatMap (_.businessEndDate map asDate)
      accountingService.calculateSubmissionDates(item.incorpDate, activeDate(details), prepDate)
    } match {
      case Some(dates) => Future.successful(dates)
      case None => Future.failed(new MissingAccountingDates)
    }
  }

  private[services] def appendDataToSubmission(crn: String, dates: SubmissionDates, partialSubmission: JsObject) : JsObject = {
    partialSubmission deepMerge
      Json.obj("registration" ->
        Json.obj("corporationTax" ->
          Json.obj(
            "crn" -> crn,
            "companyActiveDate" ->
              formatDate(
                dates.companyActiveDate),
            "startDateOfFirstAccountingPeriod" -> formatDate(dates.startDateOfFirstAccountingPeriod),
            "intendedAccountsPreparationDate" -> formatDate(dates.intendedAccountsPreparationDate)
          )
        )
      )
  }

  private[services] def postSubmissionToDes(ackRef: String, submission: JsObject) = {
    val hc = new HeaderCarrier()
    desConnector.ctSubmission(ackRef, submission)(hc)
  }
}
