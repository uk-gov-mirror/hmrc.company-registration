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

package connectors

import config.MicroserviceAppConfig
import helpers.BaseSpec
import mocks.WSHttpMock
import org.mockito.ArgumentMatchers.{any, eq => eqTo}
import org.mockito.Mockito._
import play.api.libs.json.{JsValue, Json}
import play.api.test.Helpers._
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, SessionId, UpstreamErrorResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class DesConnectorSpec extends BaseSpec with WSHttpMock {

  trait Setup {

    implicit val ec: ExecutionContext = global
    val serviceURL = "http:///test-connector-url"
    val urlHeaderEnvironment = "test"
    val urlHeaderAuthorization = "testAuth"
    val headers = Seq("Authorization" -> s"Bearer $urlHeaderAuthorization", "Environment" -> urlHeaderEnvironment)

    val brUrl = s"$serviceURL/business-registration/corporation-tax"
    val biUrl = s"$serviceURL/business-incorporation/corporation-tax"

    val mockMicroserviceAppConfig: MicroserviceAppConfig = mock[MicroserviceAppConfig]
    val connector: DesConnector = new DesConnector(mockMicroserviceAppConfig, mockWSHttp)(ec)

    when(mockMicroserviceAppConfig.desUrl).thenReturn(serviceURL)
    when(mockMicroserviceAppConfig.getConfigString(eqTo("des-service.environment"))).thenReturn(urlHeaderEnvironment)
    when(mockMicroserviceAppConfig.getConfigString(eqTo("des-service.authorization-token"))).thenReturn(urlHeaderAuthorization)

    reset(mockWSHttp)
  }

  "httpRds" must {

    "return the http response when a 200 status code is read from the http response" in new Setup {
      val response: HttpResponse = HttpResponse(200, "")
      connector.httpRds.read("http://", "testUrl", response) mustBe response
    }

    "return a not found exception when it reads a 404 status code from the http response" in new Setup {
      intercept[UpstreamErrorResponse] {
        connector.httpRds.read("http://", "testUrl", HttpResponse(404, ""))
      }
    }
  }

  "DscConnector" must {
    val submissionJson = Json.obj("x" -> "y")
    implicit val hc: HeaderCarrier = new HeaderCarrier(sessionId = Some(SessionId(s"session-${UUID.randomUUID}")))

    "for accepted submission, return success" in new Setup {
      when(mockWSHttp.POST[JsValue, HttpResponse](any(), any(), any())(any(), any(), any(),any()))
        .thenReturn(Future.successful(HttpResponse(202, submissionJson, Map())))

      val result: HttpResponse = await(connector.ctSubmission("", submissionJson, "testJID"))
      result.status mustBe 202

      verify(mockWSHttp, times(1)).POST(eqTo(brUrl), eqTo(submissionJson), eqTo(headers))(any(), any(), any(),any())
    }

    "for topup  submission, return success" in new Setup {
      when(mockWSHttp.POST[JsValue, HttpResponse](any(), any(), any())(any(), any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse(202, submissionJson, Map())))

      val result: HttpResponse = await(connector.topUpCTSubmission("", submissionJson, "testJID"))
      result.status mustBe 202

      verify(mockWSHttp, times(1)).POST(eqTo(biUrl), eqTo(submissionJson), eqTo(headers))(any(), any(), any(),any())
    }

    "for a forbidden request, return a bad request" in new Setup {
      when(mockWSHttp.POST[JsValue, HttpResponse](any(), any(), any())(any(), any(), any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse("", 403, 400)))

      intercept[UpstreamErrorResponse] {
        await(connector.ctSubmission("", submissionJson, "testJID"))
      }
      verify(mockWSHttp, times(1)).POST(eqTo(brUrl), eqTo(submissionJson), eqTo(headers))(any(), any(), any(),any())
    }

    "for a forbidden topup request, return a bad request" in new Setup {
      when(mockWSHttp.POST[JsValue, HttpResponse](any(), any(), any())(any(), any(), any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse("", 403, 400)))

      intercept[UpstreamErrorResponse] {
        await(connector.topUpCTSubmission("", submissionJson, "testJID"))
      }
      verify(mockWSHttp, times(1)).POST(eqTo(biUrl), eqTo(submissionJson), eqTo(headers))(any(), any(), any(),any())
    }


    "for a client request timeout, return unavailable" in new Setup {
      when(mockWSHttp.POST[JsValue, HttpResponse](any(), any(), any())(any(), any(), any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse("", 499, 502)))

      intercept[UpstreamErrorResponse] {
        await(connector.ctSubmission("", submissionJson, "testJID"))
      }
      verify(mockWSHttp, times(1)).POST(eqTo(brUrl), eqTo(submissionJson), eqTo(headers))(any(), any(), any(),any())
    }

    "for a client topup request timeout, return unavailable" in new Setup {
      when(mockWSHttp.POST[JsValue, HttpResponse](any(), any(), any())(any(), any(), any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse("", 499, 502)))

      intercept[UpstreamErrorResponse] {
        await(connector.topUpCTSubmission("", submissionJson, "testJID"))
      }
      verify(mockWSHttp, times(1)).POST(eqTo(biUrl), eqTo(submissionJson), eqTo(headers))(any(), any(), any(),any())
    }
  }

  "customDESRead" must {

    "return the response on an acceptable request" in new Setup {
      val response: HttpResponse = HttpResponse(202, "")
      connector.customDESRead("", "", response) mustBe response
    }

    "return a UpstreamErrorResponse on a bad request" in new Setup {
      intercept[UpstreamErrorResponse] {
        connector.customDESRead("", "", HttpResponse(400, ""))
      }
    }

    "return the HttpResponse as a 202 on a conflict" in new Setup {
      connector.customDESRead("", "", HttpResponse(409, "")).status mustBe 202
    }

    "return a UpstreamErrorResponse on a timeout" in new Setup {
      val ex: UpstreamErrorResponse = intercept[UpstreamErrorResponse] {
        connector.customDESRead("", "", HttpResponse(499, ""))
      }
      ex.reportAs mustBe 502
    }
    "return a UpstreamErrorResponse when response is 503" in new Setup {
      val ex: UpstreamErrorResponse = intercept[UpstreamErrorResponse] {
        connector.customDESRead("", "", HttpResponse(429, ""))
      }
      ex.reportAs mustBe 503
    }
  }
}
