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

import connectors.RoutingConnector
import helpers.BaseSpec
import mocks.MockMetricsService
import org.mockito.ArgumentMatchers.{any, eq => eqTo}
import org.mockito.Mockito._
import play.api.libs.json.Json
import play.api.test.Helpers._
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, SessionId, UpstreamErrorResponse}
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.http.connector.AuditResult.Success

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class SubmissionEventServiceSpec extends BaseSpec {

  trait Setup {

    implicit val ec: ExecutionContext = global

    val mockRoutingConnector: RoutingConnector = mock[RoutingConnector]
    val mockMetricsService: MetricsService = MockMetricsService
    val mockAuditConnector: AuditConnector = mock[AuditConnector]

    val mockAuditingConfig: AuditingConfig = mock[AuditingConfig]

    when(mockAuditConnector.auditingConfig) thenReturn mockAuditingConfig
    when(mockAuditingConfig.auditSource) thenReturn "company-registration"

    val connector: SubmissionEventService = new SubmissionEventService(mockRoutingConnector, mockMetricsService, mockAuditConnector)(ec)
  }

  "SubmissionEventService" must {
    val submission = Json.obj("x" -> "y")
    implicit val hc: HeaderCarrier = new HeaderCarrier(sessionId = Some(SessionId(s"session-${UUID.randomUUID}")))

    "for accepted submission, return success" in new Setup {
      when(mockRoutingConnector.ctSubmission(any(), any(), any())(eqTo(hc)))
        .thenReturn(Future.successful(HttpResponse(202, json = Json.obj("x" -> "y"), Map())))

      when(mockAuditConnector.sendExtendedEvent(any())(any(), any()))
        .thenReturn(Future.successful(Success))

      val result: HttpResponse = await(connector.ctSubmission("", submission, "testJID"))
      result.status mustBe 202
      verify(mockRoutingConnector, times(1)).ctSubmission("", submission, "testJID")
    }

    "for topup  submission, return success" in new Setup {
      when(mockRoutingConnector.topUpCTSubmission(any(), any(), any())(eqTo(hc)))
        .thenReturn(Future.successful(HttpResponse(202, json = Json.obj("x" -> "y"), Map())))

      when(mockAuditConnector.sendExtendedEvent(any())(any(), any()))
        .thenReturn(Future.successful(Success))

      val result: HttpResponse = await(connector.topUpCTSubmission("", submission, "testJID"))

      result.status mustBe 202
    }

    "for a forbidden request, return a bad request" in new Setup {
      when(mockRoutingConnector.ctSubmission(any(), any(), any())(eqTo(hc)))
        .thenReturn(Future.failed(UpstreamErrorResponse("", 403, 400)))

      when(mockAuditConnector.sendExtendedEvent(any())(any(), any()))
        .thenReturn(Future.successful(Success))

      intercept[UpstreamErrorResponse] {
        await(connector.ctSubmission("", submission, "testJID"))
      }
    }

    "for a forbidden topup request, return a bad request" in new Setup {
      when(mockRoutingConnector.topUpCTSubmission(any(), any(), any())(eqTo(hc)))
        .thenReturn(Future.failed(UpstreamErrorResponse("", 403, 400)))

      when(mockAuditConnector.sendExtendedEvent(any())(any(), any()))
        .thenReturn(Future.successful(Success))

      intercept[UpstreamErrorResponse] {
        await(connector.topUpCTSubmission("", submission, "testJID"))
      }
    }


    "for a client request timedout, return unavailable" in new Setup {
      when(mockRoutingConnector.ctSubmission(any(), any(), any())(eqTo(hc)))
        .thenReturn(Future.failed(UpstreamErrorResponse("", 499, 502)))

      when(mockAuditConnector.sendExtendedEvent(any())(any(), any()))
        .thenReturn(Future.successful(Success))

      intercept[UpstreamErrorResponse] {
        await(connector.ctSubmission("", submission, "testJID"))
      }
    }

    "for a client topup request timedout, return unavailable" in new Setup {
      when(mockRoutingConnector.topUpCTSubmission(any(), any(), any())(eqTo(hc)))
        .thenReturn(Future.failed(UpstreamErrorResponse("", 499, 502)))

      when(mockAuditConnector.sendExtendedEvent(any())(any(), any()))
        .thenReturn(Future.successful(Success))

      intercept[UpstreamErrorResponse] {
        await(connector.topUpCTSubmission("", submission, "testJID"))
      }
    }
  }

}
