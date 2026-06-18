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

import audit.{CTRegistrationSubmissionAuditEventDetails, ApiResponse, RegistrationAuditEventConstants}
import play.api.libs.json.{JsObject, Json, Writes}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions.auditHeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent

import java.time.Instant
import java.util.UUID
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class AuditServiceImpl @Inject()(val auditConnector: AuditConnector)(implicit val ec: ExecutionContext) extends AuditService

trait AuditService {

  implicit val ec: ExecutionContext

  val auditConnector: AuditConnector

  private[services] def now() = Instant.now()
  private[services] def eventId() = UUID.randomUUID().toString

  def sendEvent[T](auditType: String, detail: T, transactionName: Option[String] = None)
                  (implicit hc: HeaderCarrier, fmt: Writes[T]): Future[AuditResult] = {

    val event = ExtendedDataEvent(
      auditSource = auditConnector.auditingConfig.auditSource,
      auditType   = auditType,
      eventId     = eventId(),
      tags        = hc.toAuditTags(
        transactionName = transactionName.getOrElse(auditType),
        path = hc.otherHeaders.collectFirst { case (RegistrationAuditEventConstants.PATH, value) => value }.getOrElse("-")
      ),
      detail      = Json.toJson(detail),
      generatedAt = now()
    )

    auditConnector.sendExtendedEvent(event)
  }

  def sendCTRegSubmissionEvent(event: CTRegistrationSubmissionAuditEventDetails)(implicit hc: HeaderCarrier): Future[AuditResult] = {
    val (auditType, transactionName) = event.reason.isDefined match {
      case true => ("ctRegistrationSubmissionFailed", "CTRegistrationSubmissionFailed")
      case false => ("ctRegistrationSubmissionSuccessful", "CTRegistrationSubmission")
    }
    sendEvent(auditType, event, Some(transactionName))
  }

  def ctRegSubmissionFromJson(journeyId: String, json: JsObject): CTRegistrationSubmissionAuditEventDetails = {
    val apiResponse = json.as[ApiResponse]
    CTRegistrationSubmissionAuditEventDetails(
      journeyId,
      apiResponse.processingDate,
      apiResponse.acknowledgementReference,
      apiResponse.reason
    )
  }
}