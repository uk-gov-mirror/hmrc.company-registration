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

package controllers

import auth.{AuthorisedActions, CryptoSCRS}
import models.validation.APIValidation
import models.{AcknowledgementReferences, ConfirmationReferences}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, AnyContentAsJson, ControllerComponents, Request}
import repositories.{CorporationTaxRegistrationMongoRepository, Repositories}
import services.{MetricsService, SubmissionService}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals.credentials
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import utils.{AlertLogging, PagerDutyKeys}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubmissionController @Inject()(val metricsService: MetricsService,
                                     val authConnector: AuthConnector,
                                     val submissionService: SubmissionService,
                                     val repositories: Repositories,
                                     val alertLogging: AlertLogging,
                                     val cryptoSCRS: CryptoSCRS,
                                     controllerComponents: ControllerComponents
                                    )(implicit val ec: ExecutionContext) extends BackendController(controllerComponents) with AuthorisedActions {
  lazy val resource: CorporationTaxRegistrationMongoRepository = repositories.cTRepository

  def handleUserSubmission(registrationID: String): Action[JsValue] =
    AuthorisedAction(registrationID).retrieve(credentials).async(parse.json) { optCredentials =>
      implicit request =>
        optCredentials match {
          case Some(credentials) =>
            val requestAsAnyContentAsJson: Request[AnyContentAsJson] = request.map(AnyContentAsJson)
            withJsonBody[ConfirmationReferences] { refs =>
              val timer = metricsService.updateReferencesCRTimer.time()
              submissionService.handleSubmission(registrationID, credentials.providerId, refs, isAdmin = false)(
                hc, requestAsAnyContentAsJson) map { references =>
                timer.stop()
                logger.info(s"[Confirmation Refs] Acknowledgement ref:${references.acknowledgementReference} " +
                  s"- Transaction id:${references.transactionId} - Payment ref:${references.paymentReference}")
                Ok(Json.toJson[ConfirmationReferences](references))
              }
            }
          case _ =>
            logger.error(s"[CorporationTaxRegistrationController][handleUserSubmission] For registrationId $registrationID no Credentials were retrieved from call to Auth")
            Future.successful(Forbidden("No credentials retrieved from call to Auth"))
        }
    }

  def acknowledgementConfirmation(ackRef: String): Action[JsValue] = Action.async[JsValue](parse.json) {
    logger.debug(s"[CorporationTaxRegistrationController] [acknowledgementConfirmation] confirming for ack $ackRef")
    implicit request =>
      withJsonBody[AcknowledgementReferences] { apiNotification =>

        (apiNotification.ctUtr.isDefined, apiNotification.status) match {
          case accepted@(true, "04" | "05") =>
            val timer = metricsService.acknowledgementConfirmationCRTimer.time()
            submissionService.updateCTRecordWithAckRefs(ackRef, apiNotification) map {
              case Some(_) =>
                timer.stop()
                metricsService.ctutrConfirmationCounter.inc(1)
                Ok
              case None =>
                timer.stop()
                NotFound(s"Document not found for Ack ref: $ackRef")
            }
          case rejected@(_, "06" | "07" | "08" | "09" | "10") =>
            alertLogging.pagerduty(PagerDutyKeys.CT_REJECTED, Some(s"Received a Rejected response code (${apiNotification.status}) from Gateway for ackRef: $ackRef"))
            submissionService.updateCTRecordWithAckRefs(ackRef, apiNotification.copy(ctUtr = None)) map { _ => Ok }
          case missingCTUTR@(_, "04" | "05") =>
            alertLogging.pagerduty(PagerDutyKeys.CT_ACCEPTED_MISSING_UTR, Some(s"Received an Accepted response code (${apiNotification.status}) from Gateway without a CTUTR for ackRef: $ackRef"))
            Future.successful(BadRequest(s"Accepted but no CTUTR provided for ackRef: $ackRef"))
          case unrecognised =>
            Future.failed(new RuntimeException(s"Unknown notification code (${apiNotification.status}) received from Gateway for ackRef: $ackRef"))
        }
      }(implicitly, implicitly, AcknowledgementReferences.format(APIValidation, cryptoSCRS))
  }
}
