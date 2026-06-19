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
import mocks.WSHttpClientV2Mock
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito._
import play.api.libs.json.Json
import play.api.test.Helpers._
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, SessionId, StringContextOps, UpstreamErrorResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

class HipConnectorSpec extends BaseSpec with WSHttpClientV2Mock {

  trait Setup {

    implicit val ec: ExecutionContext = global
    val serviceURL = "http://test-connector-url"
    val hipClientId = "test"
    val hipClientSecret = "testAuth"

    val busReqUrl = url"$serviceURL/RESTAdapter/business-registration/CT"
    val busIncUrl = url"$serviceURL/RESTAdapter/business-incorporation/CT"

    val mockMicroserviceAppConfig: MicroserviceAppConfig = mock[MicroserviceAppConfig]
    val connector: HipConnector = new HipConnector(mockMicroserviceAppConfig, mockHttpClientV2)(ec)

    when(mockMicroserviceAppConfig.hipUrl).thenReturn(serviceURL)
    when(mockMicroserviceAppConfig.hipClientId).thenReturn(hipClientId)
    when(mockMicroserviceAppConfig.hipClientSecret).thenReturn(hipClientSecret)

    reset(mockHttpClientV2)
  }

  "HipConnector" must {

    val submissionJson = Json.parse("""{"x" : "y"}""")
    implicit val hc: HeaderCarrier = new HeaderCarrier(sessionId = Some(SessionId(s"session-${UUID.randomUUID}")))

    "for accepted submission, return success" in new Setup {
      mockHttpPost(busReqUrl, submissionJson, HttpResponse(202, submissionJson, Map.empty))

      val result: HttpResponse = await(connector.ctSubmission("", submissionJson, "testJID"))
      result.status mustBe 202
      verify(mockHttpClientV2, times(1)).post(eqTo(busReqUrl))(eqTo(hc))
    }

    "for topup submission, return success" in new Setup {
      mockHttpPost(busIncUrl, submissionJson, HttpResponse(202, submissionJson, Map.empty))

      val result: HttpResponse = await(connector.topUpCTSubmission("", submissionJson, "testJID"))
      result.status mustBe 202
      verify(mockHttpClientV2, times(1)).post(eqTo(busIncUrl))(eqTo(hc))
    }

    "for a forbidden request, return a bad request" in new Setup {
      mockHttpPostError(busReqUrl, submissionJson, UpstreamErrorResponse("", 403, 400))
      intercept[UpstreamErrorResponse] {
        await(connector.ctSubmission("", submissionJson, "testJID"))
      }
      verify(mockHttpClientV2, times(1)).post(eqTo(busReqUrl))(eqTo(hc))
    }

    "for a forbidden topup request, return a bad request" in new Setup {
      mockHttpPostError(busIncUrl, submissionJson, UpstreamErrorResponse("", 403, 400))
      intercept[UpstreamErrorResponse] {
        await(connector.topUpCTSubmission("", submissionJson, "testJID"))
      }
      verify(mockHttpClientV2, times(1)).post(eqTo(busIncUrl))(eqTo(hc))
    }


    "for a client request timeout, return unavailable" in new Setup {
      mockHttpPostError(busReqUrl, submissionJson, UpstreamErrorResponse("", 499, 502))  //check if HIP can produce a 499
      intercept[UpstreamErrorResponse] {
        await(connector.ctSubmission("", submissionJson, "testJID"))
      }
      verify(mockHttpClientV2, times(1)).post(eqTo(busReqUrl))(eqTo(hc))
    }

    "for a client topup request timeout, return unavailable" in new Setup {
      mockHttpPostError(busIncUrl, submissionJson, UpstreamErrorResponse("", 499, 502))  //check if HIP can produce a 499
      intercept[UpstreamErrorResponse] {
        await(connector.topUpCTSubmission("", submissionJson, "testJID"))
      }
      verify(mockHttpClientV2, times(1)).post(eqTo(busIncUrl))(eqTo(hc))
    }
  }

  "httpRds" must {

    "return the http response when a 200 status code is read from the http response" in new Setup {
      val response = HttpResponse(OK, "")
      connector.httpRds.read(GET, "testUrl", response) mustBe response
    }

    "return a not found exception when it reads a 404 status code from the http response" in new Setup {
      intercept[UpstreamErrorResponse] {
        connector.httpRds.read(POST, "testUrl", HttpResponse(NOT_FOUND, ""))
      }
    }

    "return the response on an acceptable request" in new Setup {
      val response = HttpResponse(ACCEPTED, "")
      connector.httpRds.read(POST, "testUrl", response) mustBe response
    }

    "return a UpstreamErrorResponse on a bad request" in new Setup {
      intercept[UpstreamErrorResponse] {
        connector.httpRds.read(POST, "testUrl", HttpResponse(BAD_REQUEST, ""))
      }
    }

    "return the HttpResponse as a 202 on a conflict" in new Setup {
      connector.httpRds.read(POST, "testUrl", HttpResponse(CONFLICT, "")).status mustBe ACCEPTED
    }

    "return a UpstreamErrorResponse on a timeout" in new Setup {
      intercept[UpstreamErrorResponse] {
        connector.httpRds.read(POST, "testUrl", HttpResponse(499, ""))
      }
        .reportAs mustBe BAD_GATEWAY
    }
    
    "return a UpstreamErrorResponse when response is 503" in new Setup {
      intercept[UpstreamErrorResponse] {
        connector.httpRds.read(POST, "testUrl", HttpResponse(TOO_MANY_REQUESTS, ""))
      }
      .reportAs mustBe SERVICE_UNAVAILABLE
    }
  }
}
