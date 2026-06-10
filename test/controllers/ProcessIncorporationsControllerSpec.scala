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

import models.IncorpStatus
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.concurrent.Eventually
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.Reads._
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services._
import utils.LogCapturingHelper

import java.time.{LocalDate, ZoneOffset}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ProcessIncorporationsControllerSpec extends PlaySpec with MockitoSugar with LogCapturingHelper with Eventually {

  implicit val as: ActorSystem = ActorSystem()
  implicit val mat: Materializer = Materializer(as)

  val regId = "1234"
  val incDate: LocalDate = LocalDate.parse("2000-12-12")
  val transactionId = "trans-12345"
  val crn = "crn-12345"

  val mockProcessIncorporationService: ProcessIncorporationService = mock[ProcessIncorporationService]
  val mockCorpRegTaxService: CorporationTaxRegistrationService = mock[CorporationTaxRegistrationService]
  val mockSubmissionService: SubmissionService = mock[SubmissionService]

  class Setup {
    object Controller extends ProcessIncorporationsController(mockProcessIncorporationService,
      mockCorpRegTaxService,
      mockSubmissionService,
      stubControllerComponents(playBodyParsers = stubPlayBodyParsers(mat))) {
    }
  }

  val rejectedIncorpJson: JsObject = Json.parse(
    s"""
       |{
       |  "SCRSIncorpStatus":{
       |    "IncorpSubscriptionKey":{
       |      "subscriber":"abc123",
       |      "discriminator":"CT100",
       |      "transactionId":"$transactionId"
       |    },
       |    "SCRSIncorpSubscription":{
       |      "callbackUrl":"www.testUpdate.com"
       |    },
       |    "IncorpStatusEvent":{
       |      "status":"rejected",
       |      "description":"description"
       |    }
       |  }
       |}
    """.stripMargin).as[JsObject]

  val rejectedIncorpStatus: IncorpStatus = IncorpStatus(transactionId, "rejected", None, Some("description"), None)

  val acceptedIncorpJson: JsObject = Json.parse(
    s"""
       |{
       |  "SCRSIncorpStatus":{
       |    "IncorpSubscriptionKey":{
       |      "subscriber":"abc123",
       |      "discriminator":"CT100",
       |      "transactionId":"$transactionId"
       |    },
       |    "SCRSIncorpSubscription":{
       |      "callbackUrl":"www.testUpdate.com"
       |    },
       |    "IncorpStatusEvent":{
       |       "status":"accepted",
       |      "crn":"$crn",
       |      "incorporationDate":${incDate.atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli}
       |    }
       |  }
       |}
    """.stripMargin).as[JsObject]

  val DESFailedJson: JsObject = Json.parse(
    s"""
       |{
       |}
    """.stripMargin).as[JsObject]

  val acceptedIncorpStatus: IncorpStatus = IncorpStatus(transactionId, "accepted", Some(crn), None, Some(incDate))

  "ProcessAdminIncorp" must {

    "read rejected json from request into IncorpStatus case class" in {
      rejectedIncorpJson.as[IncorpStatus](IncorpStatus.reads) mustBe rejectedIncorpStatus
    }

    "read accepted json from request into IncorpStatus case class" in {
      acceptedIncorpJson.as[IncorpStatus](IncorpStatus.reads) mustBe acceptedIncorpStatus
    }

    "return a 200 response " in new Setup {

      when(mockProcessIncorporationService.processIncorporationUpdate(any(), any())(any())).thenReturn(Future.successful(true))

      val request: FakeRequest[JsObject] = FakeRequest().withBody[JsObject](rejectedIncorpJson)

      val result: Future[Result] = call(Controller.processAdminIncorporation, request)

      status(result) mustBe 200

    }
  }

  "ProcessIncorp" must {

    "read json from request into IncorpStatus case class" in {
      rejectedIncorpJson.as[IncorpStatus](IncorpStatus.reads) mustBe rejectedIncorpStatus
    }

    "return a 200 response " in new Setup {
      when(mockProcessIncorporationService.processIncorporationUpdate(any(), any())(any())).thenReturn(Future.successful(true))

      val request: FakeRequest[JsObject] = FakeRequest().withBody[JsObject](rejectedIncorpJson)
      val result: Future[Result] = call(Controller.processIncorporationNotification, request)

      status(result) mustBe 200
    }
  }

  "Failing Topup" must {
    "log the correct error message" in new Setup {
      when(mockProcessIncorporationService.processIncorporationUpdate(any(), any())(any())).thenReturn(Future.failed(new RuntimeException))

      val request: FakeRequest[JsObject] = FakeRequest().withBody[JsObject](rejectedIncorpJson)
      withCaptureOfLoggingFrom(Controller.logger) { logEvents =>
        intercept[RuntimeException](await(call(Controller.processIncorporationNotification, request)))
        eventually {
          logEvents.size mustBe 1
          logEvents.head.getMessage mustBe "[Controller][processIncorporationNotification] FAILED_DES/HIP_TOPUP - Topup failed for transaction ID: trans-12345"
        }
      }
    }
  }

  "Invalid Data" must {

    "return a 202 response for non admin flow" in new Setup {
      when(mockProcessIncorporationService.processIncorporationUpdate(any(), any())(any())).thenReturn(Future.successful(false))
      when(mockSubmissionService.setupPartialForTopupOnLocked(any())(any(), any())).thenReturn(Future.successful(false))
      val request: FakeRequest[JsObject] = FakeRequest().withBody[JsObject](rejectedIncorpJson)
      val result: Future[Result] = call(Controller.processIncorporationNotification, request)

      status(result) mustBe 202
    }


    "return a 500 response for admin flow" in new Setup {
      when(mockProcessIncorporationService.processIncorporationUpdate(any(), any())(any())).thenReturn(Future.successful(false))
      val request: FakeRequest[JsObject] = FakeRequest().withBody[JsObject](rejectedIncorpJson)
      val result: Future[Result] = call(Controller.processAdminIncorporation, request)

      status(result) mustBe 400

    }
  }

}
