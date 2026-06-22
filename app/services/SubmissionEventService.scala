/*
 * Copyright 2026 HM Revenue & Customs
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

import audit.RegistrationAuditEventConstants.JOURNEY_ID
import connectors.RoutingConnector
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.http._
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import utils.Logging

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SubmissionEventService @Inject()(
                                        routingConnector: RoutingConnector,
                                        metricsService: MetricsService,
                                        val auditConnector: AuditConnector
                                )(implicit val ec: ExecutionContext) extends AuditService with HttpErrorFunctions with Logging  {

  def ctSubmission(ackRef: String, submission: JsObject, journeyId: String)(implicit headerCarrier: HeaderCarrier): Future[HttpResponse] = {
    metricsService.processDataResponseWithMetrics[HttpResponse](metricsService.apiSubmissionCrtTimer.time()) {
      routingConnector.ctSubmission(ackRef, submission, journeyId) map { response =>
        sendCTRegSubmissionEvent(ctRegSubmissionFromJson(journeyId, response.json.as[JsObject]))
        response
      } recoverWith {
        case ex: UpstreamErrorResponse if UpstreamErrorResponse.Upstream4xxResponse.unapply(ex).isDefined =>
          sendEvent("ctRegistrationSubmissionFailed", Json.obj("submission" -> submission, JOURNEY_ID -> journeyId))
          throw ex
      }
    }
  }

  def topUpCTSubmission(ackRef: String, submission: JsObject, journeyId: String)(implicit headerCarrier: HeaderCarrier): Future[HttpResponse] = {
    metricsService.processDataResponseWithMetrics[HttpResponse](metricsService.apiSubmissionCrtTimer.time()) {
      routingConnector.topUpCTSubmission(ackRef, submission, journeyId) map { response =>
        sendCTRegSubmissionEvent(ctRegSubmissionFromJson(journeyId, response.json.as[JsObject]))
        response
      } recoverWith {
        case ex: UpstreamErrorResponse if UpstreamErrorResponse.Upstream4xxResponse.unapply(ex).isDefined =>
          sendEvent("ctRegistrationSubmissionFailed", Json.obj("submission" -> submission, JOURNEY_ID -> journeyId)) //ctTopupRegistrationSubmissionFailed ???
          throw ex
      }
    }
  }

}
