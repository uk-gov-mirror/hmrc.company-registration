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

import models._
import utils.Logging
import play.api.libs.json.{JsValue, Reads}
import play.api.mvc.{Action, AnyContentAsJson, ControllerComponents, Request}
import services._
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ProcessIncorporationsController @Inject()(val processIncorporationService: ProcessIncorporationService,
                                                val corpTaxRegService: CorporationTaxRegistrationService,
                                                val submissionService: SubmissionService,
                                                controllerComponents: ControllerComponents
                                               )(implicit val ec: ExecutionContext) extends BackendController(controllerComponents) with Logging {

  private def logFailedTopup(txId: String, method: String): Unit =
    logger.error(s"[$method] FAILED_DES/HIP_TOPUP - Topup failed for transaction ID: $txId")

  def processIncorporationNotification: Action[JsValue] = Action.async[JsValue](parse.json) {
    implicit request =>

      implicit val reads: Reads[IncorpStatus] = IncorpStatus.reads
      withJsonBody[IncorpStatus] { incorp =>
        val requestAsAnyContentAsJson: Request[AnyContentAsJson] = request.map(AnyContentAsJson)
        processIncorporationService.processIncorporationUpdate(incorp.toIncorpUpdate) flatMap {
          if (_) Future.successful(Ok) else {
            submissionService.setupPartialForTopupOnLocked(incorp.transactionId)(hc, requestAsAnyContentAsJson) map { _ =>
              logger.info(s"[processIncorporationNotification] Sent partial submission in response to locked document on incorp update: ${incorp.transactionId}")
              Accepted
            }
          }
        } recover {
          case NoSessionIdentifiersInDocument => Ok
          case e =>
            logFailedTopup(incorp.transactionId, "processIncorporationNotification")
            throw e
        }
      }
  }

  def processAdminIncorporation: Action[JsValue] = Action.async[JsValue](parse.json) {
    implicit request =>
      implicit val reads: Reads[IncorpStatus] = IncorpStatus.reads
      withJsonBody[IncorpStatus] { incorp =>
        processIncorporationService.processIncorporationUpdate(incorp.toIncorpUpdate, isAdmin = true) map {
          if (_) {
            Ok
          } else {
            logFailedTopup(incorp.transactionId, "processAdminIncorporation")
            BadRequest
          }
        } recover {
          case e =>
            logFailedTopup(incorp.transactionId, "processAdminIncorporation")
            throw e
        }
      }
  }

}
