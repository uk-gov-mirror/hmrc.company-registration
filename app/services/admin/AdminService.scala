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

package services.admin

import audit._
import config.MicroserviceAppConfig
import connectors.{BusinessRegistrationConnector, IncorporationInformationConnector}
import helpers.DateFormatter
import jobs.{LockResponse, MongoLocked, ScheduledService, UnlockingFailed}
import models.RegistrationStatus._
import models.{ConfirmationReferences, CorporationTaxRegistration, HO6RegistrationInformation, SessionIdData}
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Request
import repositories.{CorporationTaxRegistrationMongoRepository, Repositories}
import services.{AuditService, FailedToDeleteSubmissionData, SubmissionEventService}
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException}
import uk.gov.hmrc.mongo.lock.LockService
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import utils.Logging

import java.time.Instant
import java.util.Base64
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AdminServiceImpl @Inject()(val corpTaxRegRepo: CorporationTaxRegistrationMongoRepository,
                                 val brConnector: BusinessRegistrationConnector,
                                 val desConnector: SubmissionEventService,
                                 val repositories: Repositories,
                                 val incorpInfoConnector: IncorporationInformationConnector, microserviceAppConfig: MicroserviceAppConfig,
                                 val auditService: AuditService,
                                 servicesConfig: ServicesConfig
                                )(implicit val ec: ExecutionContext)
  extends AdminService {
  lazy val staleAmount: Int = servicesConfig.getInt("staleDocumentAmount")
  lazy val clearAfterXDays: Int = servicesConfig.getInt("clearAfterXDays")
  lazy val ignoredDocs: Set[String] = new String(Base64.getDecoder.decode(microserviceAppConfig.getConfigString("skipStaleDocs")), "UTF-8").split(",").toSet

  lazy val lockoutTimeout: Int = servicesConfig.getInt("schedules.remove-stale-documents-job.lockTimeout")
  lazy val lockKeeper: LockService = LockService(repositories.lockRepository, "remove-stale-documents-job-lock", lockoutTimeout.seconds)
}

trait AdminService extends ScheduledService[Either[Int, LockResponse]] with DateFormatter with Logging {

  implicit val ec: ExecutionContext
  val corpTaxRegRepo: CorporationTaxRegistrationMongoRepository
  val desConnector: SubmissionEventService
  val auditService: AuditService
  val incorpInfoConnector: IncorporationInformationConnector
  val brConnector: BusinessRegistrationConnector
  val lockKeeper: LockService

  val staleAmount: Int
  val clearAfterXDays: Int
  val ignoredDocs: Set[String]

  def fetchHO6RegistrationInformation(regId: String): Future[Option[HO6RegistrationInformation]] = corpTaxRegRepo.fetchHO6Information(regId)

  def fetchSessionIdData(regId: String): Future[Option[SessionIdData]] = {
    corpTaxRegRepo.findOneBySelector(corpTaxRegRepo.regIDSelector(regId)) map (_.map { reg =>
      SessionIdData(
        reg.sessionIdentifiers.map(_.sessionId),
        reg.sessionIdentifiers.map(_.credId),
        reg.companyDetails.map(_.companyName),
        reg.confirmationReferences.map(_.acknowledgementReference)
      )
    })
  }

  private[services] def forceSubscription(regId: String, transactionId: String)(implicit hc: HeaderCarrier, req: Request[_]): Future[Boolean] = {
    incorpInfoConnector.registerInterest(regId, transactionId, true)
  }

  private[services] def fetchTransactionId(regId: String): Future[Option[String]] = corpTaxRegRepo.retrieveConfirmationReferences(regId).map(_.fold[Option[String]] {
    logger.error(s"[fetchTransactionId] - Held submission found but transaction Id missing for regId $regId")
    None
  }(refs => Option(refs.transactionId)))

  def ctutrCheck(id: String): Future[JsObject] = {
    corpTaxRegRepo.retrieveStatusAndExistenceOfCTUTR(id) map {
      case Some((status, ctutr)) => Json.obj("status" -> status, "ctutr" -> ctutr)
      case _ => Json.obj()
    }
  }

  def adminDeleteSubmission(info: DocumentInfo, txId: Option[String])(implicit hc: HeaderCarrier): Future[Boolean] = {
    val cancelSub = txId match {
      case Some(tId) =>
        incorpInfoConnector.cancelSubscription(info.regId, tId) recoverWith {
          case ex: NotFoundException =>
            logger.info(s"[processStaleDocument] Registration ${info.regId} - $tId does not have CTAX subscription. Now trying to delete CT sub.")
            incorpInfoConnector.cancelSubscription(info.regId, tId, useOldRegime = true) recoverWith {
              case e: NotFoundException =>
                logger.warn(s"[processStaleDocument] Registration ${info.regId} - $tId has no subscriptions.")
                Future.successful(true)
            }
        }
      case _ => Future.successful(true)
    }

    for {
      _ <- cancelSub
      metadataDeleted <- brConnector.adminRemoveMetadata(info.regId)
      ctDeleted <- corpTaxRegRepo.removeTaxRegistrationById(info.regId)
    } yield {
      if (ctDeleted && metadataDeleted) {
        logger.info(s"[processStaleDocument] Deleted stale regId: ${info.regId} timestamp: ${info.lastSignedIn}")
        true
      } else {
        throw FailedToDeleteSubmissionData
      }
    }
  }


  def updateTransactionId(updateFrom: String, updateTo: String): Future[Boolean] = {
    corpTaxRegRepo.updateTransactionId(updateFrom, updateTo) map {
      _ == updateTo
    } recover {
      case _ => false
    }
  }

  def invoke(implicit ec: ExecutionContext): Future[Either[Int, LockResponse]] = {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    lockKeeper.withLock(deleteStaleDocuments()).map {
      case None => Right(MongoLocked)
      case Some(res) =>
        logger.info(s"[invoke] Successfully deleted $res stale documents")
        Left(res)
    }.recover {
      case e: Exception => logger.error(s"[invoke] Error running deleteStaleDocuments with message: ${e.getMessage}")
        Right(UnlockingFailed)
    }
  }


  def deleteStaleDocuments(): Future[Int] = {
    val startTS = System.currentTimeMillis
    for {
      documents <- corpTaxRegRepo.retrieveStaleDocuments(staleAmount, clearAfterXDays)
      _ = logger.info(s"[deleteStaleDocuments] Mongo query found ${documents.size} stale documents. Now processing.")
      processed <- Future.sequence(documents filterNot (doc => ignoredDocs(doc.registrationID)) map processStaleDocument)
    } yield {
      val duration = System.currentTimeMillis - startTS
      val res = processed count (_ == true)
      logger.info(s"[deleteStaleDocuments] Duration to run $duration ms")
      res
    }
  }

  case class DocumentInfo(regId: String, status: String, lastSignedIn: Instant)

  private def removeStaleDocument(documentInfo: DocumentInfo, optRefs: Option[ConfirmationReferences])(implicit hc: HeaderCarrier) = optRefs match {
    case Some(confRefs) => checkNotIncorporated(documentInfo, confRefs) flatMap { _ =>
      rejectNoneDraft(documentInfo, confRefs) flatMap { _ =>
        adminDeleteSubmission(documentInfo, Some(confRefs.transactionId))
      }
    }
    case None => adminDeleteSubmission(documentInfo, None)
  }

  private[services] def processStaleDocument(doc: CorporationTaxRegistration): Future[Boolean] = {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    val documentInfo = DocumentInfo(doc.registrationID, doc.status, doc.lastSignedIn)

    logger.info(s"[processStaleDocument] Processing stale document of $documentInfo")

    ((doc.status, doc.confirmationReferences) match {
      case (DRAFT | HELD | LOCKED, optRefs) => removeStaleDocument(documentInfo, optRefs)
      case _ => Future.successful(false)
    }) recover {
      case e: Throwable =>
        logger.warn(s"[processStaleDocument] Failed to delete regId: ${documentInfo.regId} with throwable ${e.getMessage}")
        false
    }
  }

  private def checkNotIncorporated(documentInfo: DocumentInfo, confRefs: ConfirmationReferences)(implicit hc: HeaderCarrier): Future[Boolean] = {
    incorpInfoConnector.checkCompanyIncorporated(confRefs.transactionId) map { res =>
      res.fold {
        true
      } { crn =>
        logger.warn(s"[checkNotIncorporated] Could not delete document with CRN: $crn " +
          s"regId: ${documentInfo.regId} transID: ${confRefs.transactionId} lastSignIn: ${documentInfo.lastSignedIn} status: ${documentInfo.status} paymentRefExists = ${confRefs.paymentReference.isDefined} paymentAmoutExists = ${confRefs.paymentAmount.isDefined} acknowledgmentReference is NOT empty = ${confRefs.acknowledgementReference.nonEmpty}"
        )
        throw new Exception("No deletion carried out because CRN exists")
      }
    }
  }

  private def rejectNoneDraft(info: DocumentInfo, confRefs: ConfirmationReferences)(implicit hc: HeaderCarrier) = {
    def submission(ackRef: String) = Json.obj(
      "status" -> "Rejected",
      "acknowledgementReference" -> ackRef
    )

    if (info.status != DRAFT) {
      val ackRef = confRefs.acknowledgementReference
      for {
        _ <- desConnector.topUpCTSubmission(ackRef, submission(ackRef), info.regId)
        _ <- auditService.sendEvent("ctRegistrationAdditionalData", DesTopUpSubmissionEventDetail(
          info.regId,
          ackRef,
          "Rejected",
          None,
          None,
          None,
          None,
          Some(true)
        ))
      } yield true
    } else {
      Future.successful(true)
    }
  }
}
