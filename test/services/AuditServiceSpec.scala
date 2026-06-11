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

import audit.{CTRegistrationSubmissionAuditEventDetails, FailedIncorporationAuditEventDetail}
import org.mockito.ArgumentMatchers
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsObject, Json}
import play.api.test.DefaultAwaitTimeout
import play.api.test.Helpers.await
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions.auditHeaderCarrier
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent

import java.time.Instant
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

class AuditServiceSpec extends PlaySpec with MockitoSugar with DefaultAwaitTimeout {

  val mockAuditConnector: AuditConnector = mock[AuditConnector]

  implicit val hc: HeaderCarrier = HeaderCarrier()

  class Setup(otherHcHeaders: Seq[(String, String)] = Seq()) {

    implicit val hc: HeaderCarrier = HeaderCarrier(otherHeaders = otherHcHeaders)
    implicit val ec: ExecutionContextExecutor = ExecutionContext.global

    val mockAuditConnector: AuditConnector = mock[AuditConnector]
    val mockAuditingConfig: AuditingConfig = mock[AuditingConfig]

    val instantNow: Instant = Instant.now()
    val appName = "business-registration-notification"
    val auditType = "testAudit"
    val testEventId: String = UUID.randomUUID().toString
    val txnName = "transactionName"

    when(mockAuditConnector.auditingConfig) thenReturn mockAuditingConfig
    when(mockAuditingConfig.auditSource) thenReturn appName

    val event: FailedIncorporationAuditEventDetail = FailedIncorporationAuditEventDetail("journeyId", "REJECTED")

    object TestService extends AuditService {

      implicit val ec: ExecutionContext = global
      override val auditConnector: AuditConnector = mockAuditConnector

      override private[services] def now() = instantNow
      override private[services] def eventId() = testEventId
    }
  }

  "ctRegSubmissionFromJson" must {
    "construct a CTRegistrationSubmissionAuditEventDetails with ackRef and processingDate defined" when {
      "given a successful ETMP Response" in new Setup {
        val etmpResponse: JsObject = Json.obj(
          "processingDate" -> "testDate",
          "acknowledgementReference" -> "testAckRef"
        )

        val result: CTRegistrationSubmissionAuditEventDetails = TestService.ctRegSubmissionFromJson("testJourneyId", etmpResponse)

        result.processingDate mustBe Some("testDate")
        result.acknowledgementReference mustBe Some("testAckRef")
        result.reason mustBe None
      }
    }

    "construct a CTRegistrationSubmissionAuditEventDetails with only the reason defined" when {
      "given a failed ETMP Response" in new Setup {
        val etmpResponse: JsObject = Json.obj(
          "reason" -> "testReason"
        )

        val result: CTRegistrationSubmissionAuditEventDetails = TestService.ctRegSubmissionFromJson("testJourneyId", etmpResponse)

        result.processingDate mustBe None
        result.acknowledgementReference mustBe None
        result.reason mustBe Some("testReason")
      }
    }
  }

  ".sendEvent" when {

    "call to AuditConnector is successful" when {

      "transactionName is provided and path does NOT exist" must {

        "create and send an Explicit ExtendedAuditEvent including the transactionName with pathTag set to '-'" in new Setup {

          when(
            mockAuditConnector.sendExtendedEvent(
              ArgumentMatchers.eq(ExtendedDataEvent(
                auditSource = appName,
                auditType = auditType,
                eventId = testEventId,
                tags = hc.toAuditTags(txnName, "-"),
                detail = Json.toJson(event),
                generatedAt = instantNow
              ))
            )(
              ArgumentMatchers.eq(hc),
              ArgumentMatchers.eq(ec)
            )
          ) thenReturn Future.successful(AuditResult.Success)

          val actual: AuditResult = await(TestService.sendEvent(auditType, event, Some(txnName)))

          actual mustBe AuditResult.Success
        }
      }

      "transactionName is NOT provided and path exists" must {

        "create and send an Explicit ExtendedAuditEvent with transactionName as auditType & pathTag extracted from the HC" in new Setup(
          otherHcHeaders = Seq("path" -> "/wizz/foo/bar")
        ) {

          when(
            mockAuditConnector.sendExtendedEvent(
              ArgumentMatchers.eq(ExtendedDataEvent(
                auditSource = appName,
                auditType = auditType,
                eventId = testEventId,
                tags = hc.toAuditTags(auditType, "/wizz/foo/bar"),
                detail = Json.toJson(event),
                generatedAt = instantNow
              ))
            )
            (
              ArgumentMatchers.eq(hc),
              ArgumentMatchers.eq(ec)
            )
          ) thenReturn Future.successful(AuditResult.Success)

          val actual: AuditResult = await(TestService.sendEvent(auditType, event, None))

          actual mustBe AuditResult.Success
        }
      }
    }

    "call to AuditConnector fails" must {

      "throw the exception" in new Setup {

        val exception = new Exception("Oh No")

        when(
          mockAuditConnector.sendExtendedEvent(
            ArgumentMatchers.eq(ExtendedDataEvent(
              auditSource = appName,
              auditType = auditType,
              eventId = testEventId,
              tags = hc.toAuditTags(txnName, "-"),
              detail = Json.toJson(event),
              generatedAt = instantNow
            ))
          )
          (
            ArgumentMatchers.eq(hc),
            ArgumentMatchers.eq(ec)
          )
        ) thenReturn Future.failed(exception)

        val actual: Exception = intercept[Exception](await(TestService.sendEvent(auditType, event, Some(txnName))))

        actual.getMessage mustBe exception.getMessage
      }
    }
  }
}
