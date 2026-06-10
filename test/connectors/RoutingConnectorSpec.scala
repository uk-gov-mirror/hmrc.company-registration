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

package connectors

import config.MicroserviceAppConfig
import helpers.BaseSpec
import org.mockito.ArgumentMatchers.{any, anyString, eq => eqTo}
import org.mockito.Mockito._
import play.api.Configuration
import play.api.libs.json.{JsObject, JsValue, Json}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.Future

class RoutingConnectorSpec extends BaseSpec {
  implicit val hc: HeaderCarrier = HeaderCarrier()

  val mockHipConnector: HipConnector = mock[HipConnector]
  val mockDesConnector: DesConnector = mock[DesConnector]

  val mockMicroserviceAppConfig: MicroserviceAppConfig = mock[MicroserviceAppConfig]

  "RoutingConnector" must {
    val submissionJson = Json.obj("x" -> "y")

    "delegate to the HIP connector when useHip flag is true" in {
      val routingConnector = new RoutingConnector(mockMicroserviceAppConfig, mockDesConnector, mockHipConnector)
      when(mockMicroserviceAppConfig.useHip).thenReturn(true)
      when(mockHipConnector.ctSubmission(anyString(), any[JsValue], anyString())(eqTo(hc)))
        .thenReturn(Future.successful(HttpResponse(202, submissionJson, Map())))

      routingConnector.ctSubmission("", submissionJson, "testJID")
      verify(mockHipConnector, times(1)).ctSubmission(eqTo(""), eqTo(submissionJson), eqTo("testJID"))(eqTo(hc))
    }

    "delegate to the DES connector when useHip flag is false" in {
      val routingConnector = new RoutingConnector(mockMicroserviceAppConfig, mockDesConnector, mockHipConnector)
      when(mockMicroserviceAppConfig.useHip).thenReturn(false)
      when(mockDesConnector.topUpCTSubmission(anyString(), any[JsObject], anyString())(eqTo(hc)))
        .thenReturn(Future.successful(HttpResponse(202, submissionJson, Map())))

      routingConnector.topUpCTSubmission("", submissionJson, "testJID")
      verify(mockDesConnector, times(1)).topUpCTSubmission(eqTo(""), eqTo(submissionJson), eqTo("testJID"))(eqTo(hc))
    }
  }

}
