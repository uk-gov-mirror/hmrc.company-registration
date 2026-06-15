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

import audit._
import config.MicroserviceAppConfig
import connectors.{BusinessRegistrationConnector, EmailErrorResponse, IncorporationCheckAPIConnector}
import helpers.DateHelper
import models._
import play.api.libs.json.{JsObject, Json}
import repositories.{CorporationTaxRegistrationMongoRepository, Repositories}
import uk.gov.hmrc.http.{HeaderCarrier, HttpErrorFunctions}
import utils.{DateCalculators, Logging, PagerDutyKeys}

import java.time.{Instant, LocalDate, LocalTime}
import java.util.Base64
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NoStackTrace

class ProcessIncorporationServiceImpl @Inject()(val submissionEventService: SubmissionEventService,
                                                val incorporationCheckAPIConnector: IncorporationCheckAPIConnector,
                                                val accountingService: AccountingDetailsService,
                                                val brConnector: BusinessRegistrationConnector,
                                                val sendEmailService: SendEmailService,
                                                val repositories: Repositories,
                                                val auditService: AuditService,
                                                val microserviceAppConfig: MicroserviceAppConfig
                                               )(implicit val ec: ExecutionContext) extends ProcessIncorporationService {

  lazy val ctRepository: CorporationTaxRegistrationMongoRepository = repositories.cTRepository
  lazy val addressLine4FixRegID: String = microserviceAppConfig.getConfigString("address-line-4-fix.regId")
  lazy val amendedAddressLine4: String = microserviceAppConfig.getConfigString("address-line-4-fix.address-line-4")
  lazy val blockageLoggingDay: String = microserviceAppConfig.getConfigString("check-submission-job.schedule.blockage-logging-day")
  lazy val blockageLoggingTime: String = microserviceAppConfig.getConfigString("check-submission-job.schedule.blockage-logging-time")
}

private[services] class MissingAckRef(val message: String) extends NoStackTrace

private[services] class UnexpectedStatus(val status: String) extends NoStackTrace

private[services] object FailedToUpdateSubmissionWithRejectedIncorp extends NoStackTrace

private[services] object FailedToDeleteSubmissionData extends NoStackTrace

trait ProcessIncorporationService extends DateHelper with HttpErrorFunctions with Logging {

  implicit val ec: ExecutionContext
  val submissionEventService: SubmissionEventService
  val incorporationCheckAPIConnector: IncorporationCheckAPIConnector
  val ctRepository: CorporationTaxRegistrationMongoRepository
  val accountingService: AccountingDetailsService
  val brConnector: BusinessRegistrationConnector
  val auditService: AuditService
  val sendEmailService: SendEmailService
  val microserviceAppConfig: MicroserviceAppConfig

  val addressLine4FixRegID: String
  val amendedAddressLine4: String
  val blockageLoggingDay: String
  val blockageLoggingTime: String


  private[services] class MissingAccountingDates extends NoStackTrace

  def deleteRejectedSubmissionData(regId: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    for {
      ctDeleted <- ctRepository.removeTaxRegistrationById(regId)
      metadataDeleted <- brConnector.removeMetadata(regId)
    } yield {
      if (ctDeleted && metadataDeleted) {
        logger.info(s"[deleteRejectedSubmissionData] Successfully deleted registration with regId: $regId")
        true
      } else {
        logger.error(s"[deleteRejectedSubmissionData] Failed to delete registration with regId: $regId")
        throw FailedToDeleteSubmissionData
      }
    }
  }

  private def sendEmail(ctReg: CorporationTaxRegistration)(implicit hc: HeaderCarrier): Future[Boolean] =
    sendEmailService.sendVATEmail(ctReg.verifiedEmail.get.address, ctReg.registrationID) recover {
      case _: EmailErrorResponse => true
    }

  def processIncorporationUpdate(item: IncorpUpdate, isAdmin: Boolean = false)(implicit hc: HeaderCarrier): Future[Boolean] = {
    ctRepository.findOneBySelector(ctRepository.transIdSelector(item.transactionId)) flatMap { oCTReg =>
      item.status match {
        case "accepted" =>
          oCTReg.fold {
            if (inWorkingHours) {
              logger.error(PagerDutyKeys.CT_ACCEPTED_NO_REG_DOC_II_SUBS_DELETED.toString)
            }
            logger.error(s"[processIncorporationUpdate] Incorporation accepted but no reg document found for txId: ${item.transactionId} - II subscription deleted")
            Future.successful(true)
          } { ctReg =>
            for {
              resultOfUpdate <- updateSubmissionWithIncorporation(item, ctReg, isAdmin)
              _ <- if (resultOfUpdate && ctReg.verifiedEmail.isDefined) sendEmail(ctReg) else Future.successful(true)
            } yield resultOfUpdate
          }
        case "rejected" =>
          val reason = item.statusDescription.fold(" No reason given")(f => " Reason given: " + f)
          logger.info("[processIncorporationUpdate] Incorporation rejected for Transaction: " + item.transactionId + reason)

          oCTReg.fold {
            logger.warn(s"[processIncorporationUpdate] Rejection with no CT document for trans id ${item.transactionId}")
            Future.successful(true)
          } { ctReg => updateSubmissionWithRejectedIncorp(item, ctReg, isAdmin) }
      }
    }
  }

  private[services] def updateSubmissionWithIncorporation(item: IncorpUpdate, ctReg: CorporationTaxRegistration, isAdmin: Boolean = false)(implicit hc: HeaderCarrier): Future[Boolean] = {
    import RegistrationStatus.{ACKNOWLEDGED, HELD, LOCKED, SUBMITTED}
    ctReg.status match {
      case LOCKED =>
        logger.warn(s"[updateSubmissionWithIncorporation] Top-up delayed on LOCKED document. Sending partial first. TxId:${item.transactionId} regId: ${ctReg.registrationID}.")
        Future.successful(false)
      case HELD => updateHeldSubmission(item, ctReg, ctReg.registrationID, isAdmin)
      case SUBMITTED | ACKNOWLEDGED => Future.successful(true)
      case unknown =>
        val errMsg = s"""Tried to process a submission (${ctReg.registrationID}/${item.transactionId}) with an unexpected status of $unknown"""
        logger.error(errMsg)
        Future.failed(new Exception(errMsg))
    }
  }

  private[services] def updateSubmissionWithRejectedIncorp(item: IncorpUpdate, ctReg: CorporationTaxRegistration, isAdmin: Boolean = false)
                                                          (implicit hc: HeaderCarrier): Future[Boolean] = {
    import RegistrationStatus.LOCKED
    ctReg.status match {
      case LOCKED =>
        logger.warn(s"[updateSubmissionWithRejectedIncorp] Rejection top-up delayed on LOCKED document. Sending partial first. TxId:${item.transactionId} regId: ${ctReg.registrationID}.")
        Future.successful(false)
      case _ =>
        for {
          _ <- auditFailedIncorporation(item, ctReg)
          _ <- processIncorporationRejectionSubmission(item, ctReg, ctReg.registrationID, isAdmin)
          crRejected <- ctRepository.updateSubmissionStatus(ctReg.registrationID, "rejected")
          crCleaned <- ctRepository.removeUnnecessaryRegistrationInformation(ctReg.registrationID)
        } yield {
          if (crRejected == "rejected" && crCleaned) true else throw FailedToUpdateSubmissionWithRejectedIncorp
        }
    }
  }

  private[services] def updateHeldSubmission(item: IncorpUpdate, ctReg: CorporationTaxRegistration, journeyId: String, isAdmin: Boolean = false)(implicit hc: HeaderCarrier): Future[Boolean] = {
    ctReg.confirmationReferences map (_.acknowledgementReference) match {
      case Some(ackRef) =>
        (
          for {
            submission <- constructTopUpSubmission(item, ctReg, ackRef)
            _ <- processAcceptedSubmission(item, ackRef, ctReg, submission, isAdmin)
            submitted <- ctRepository.updateHeldToSubmitted(ctReg.registrationID, item.crn.get, formatTimestamp(Instant.now()))
          } yield submitted
          ) recover {
          case e =>
            logger.error(s"[updateHeldSubmission] Submission to ${microserviceAppConfig.apiRoute}" +
              s" failed for ack ref $ackRef. Corresponding RegID: $journeyId and Transaction ID: ${item.transactionId}")
            throw e
        }
      case None =>
        val errMsg = s"""[updateHeldSubmission] Held Registration doc is missing the ack ref for tx_id "${item.transactionId}"."""
        logger.error(errMsg)
        Future.failed(new MissingAckRef(errMsg))
    }
  }

  private[services] def processAcceptedSubmission(item: IncorpUpdate, ackRef: String, ctReg: CorporationTaxRegistration,
                                                  submission: JsObject, isAdmin: Boolean = false)(implicit hc: HeaderCarrier): Future[Boolean] = {
    for {
      submitted <- submissionEventService.topUpCTSubmission(ackRef, submission, ctReg.registrationID) map (_ => true)
      _ = auditSuccessfulTopUp(submission, item, ctReg)
    } yield submitted
  }

  private[services] def processIncorporationRejectionSubmission(item: IncorpUpdate, ctReg: CorporationTaxRegistration,
                                                                journeyId: String, isAdmin: Boolean = false)(implicit hc: HeaderCarrier): Future[Boolean] = {
    ctReg.confirmationReferences map (_.acknowledgementReference) match {
      case Some(ackRef) =>
        val rejectedTopUp = Json.obj(
          "status" -> "Rejected",
          "acknowledgementReference" -> ackRef
        )
        submissionEventService.topUpCTSubmission(ackRef, rejectedTopUp, ctReg.registrationID) map { _ =>
          auditSuccessfulTopUp(rejectedTopUp, item, ctReg)
          true
        }
      case None =>
        val errMsg = s"""[processIncorporationRejectionSubmission] Held Registration doc is missing the ack ref for tx_id "${item.transactionId}"."""
        logger.error(errMsg)
        Future.failed(new MissingAckRef(errMsg))
    }
  }

  private[services] def auditSuccessfulTopUp(submission: JsObject, item: IncorpUpdate, ctReg: CorporationTaxRegistration)(implicit hc: HeaderCarrier) =
    item.incorpDate match {
      case Some(_) =>
        calculateDates(item, ctReg.accountingDetails, ctReg.accountsPreparation) flatMap { dates =>
          auditService.sendEvent("ctRegistrationAdditionalData", ApiTopUpSubmissionEventDetail(
            ctReg.registrationID,
            ctReg.confirmationReferences.get.acknowledgementReference,
            "Accepted",
            Some(dates.intendedAccountsPreparationDate),
            Some(dates.startDateOfFirstAccountingPeriod),
            Some(dates.companyActiveDate),
            item.crn
          ))
        }
      case None =>
        auditService.sendEvent("ctRegistrationAdditionalData", ApiTopUpSubmissionEventDetail(
          ctReg.registrationID,
          ctReg.confirmationReferences.get.acknowledgementReference,
          "Rejected",
          None,
          None,
          None,
          None
        ))
    }

  private[services] def auditFailedIncorporation(item: IncorpUpdate, ctReg: CorporationTaxRegistration)(implicit hc: HeaderCarrier) =
    auditService.sendEvent("failedIncorpInformation", FailedIncorporationAuditEventDetail(
      ctReg.registrationID,
      item.statusDescription.getOrElse("No reason provided")
    ))

  private def constructTopUpSubmission(item: IncorpUpdate, ctReg: CorporationTaxRegistration, ackRef: String): Future[JsObject] = {
    for {
      dates <- calculateDates(item, ctReg.accountingDetails, ctReg.accountsPreparation)
    } yield {
      Json.obj(
        "status" -> "Accepted",
        "acknowledgementReference" -> ackRef) ++
        Json.obj("corporationTax" ->
          Json.obj(
            "crn" -> item.crn.get,
            "companyActiveDate" ->
              formatDate(
                dates.companyActiveDate),
            "startDateOfFirstAccountingPeriod" -> formatDate(dates.startDateOfFirstAccountingPeriod),
            "intendedAccountsPreparationDate" -> formatDate(dates.intendedAccountsPreparationDate)
          )
        )
    }
  }

  def inWorkingHours: Boolean = {
    DateCalculators.loggingDay(blockageLoggingDay, DateCalculators.getCurrentDay) &&
      DateCalculators.loggingTime(blockageLoggingTime, LocalTime.now)
  }

  private[services] def activeDate(date: AccountingDetails, incorpDate: LocalDate) = {
    import AccountingDetails.WHEN_REGISTERED

    (date.status, date.activeDate) match {
      case (_, Some(givenDate)) if asDate(givenDate).isBefore(incorpDate) => ActiveOnIncorporation
      case (_, Some(givenDate)) => ActiveInFuture(asDate(givenDate))
      case (`WHEN_REGISTERED`, _) => ActiveOnIncorporation
      case _ => DoNotIntendToTrade
    }
  }

  private[services] def addressLine4Fix(regId: String, held: JsObject): JsObject = {
    if (regId == addressLine4FixRegID) {
      val decodedAddressLine4 = new String(Base64.getDecoder.decode(amendedAddressLine4), "UTF-8")
      held.deepMerge(
        Json.obj("registration" ->
          Json.obj("corporationTax" ->
            Json.obj("businessAddress" ->
              Json.obj("line4" -> decodedAddressLine4.mkString)))))
    } else {
      held
    }
  }

  private[services] def calculateDates(item: IncorpUpdate,
                                       accountingDetails: Option[AccountingDetails],
                                       accountsPreparation: Option[AccountPrepDetails]): Future[SubmissionDates] = {

    item.incorpDate.fold(logger.error(s"[IncorpUpdate] - Incorp update for transaction-id : ${item.transactionId} does not contain an incorp date - IncorpUpdate : $item"))(_ => ())

    accountingDetails map { details =>
      val prepDate = accountsPreparation flatMap (_.endDate)
      accountingService.calculateSubmissionDates(item.incorpDate.get, activeDate(details, item.incorpDate.get), prepDate)
    } match {
      case Some(dates) => Future.successful(dates)
      case None => Future.failed(new MissingAccountingDates)
    }
  }

}
