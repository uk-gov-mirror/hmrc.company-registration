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

package audit

import models.api.{BusinessAddress, BusinessContactDetails}
import play.api.libs.json._

case class SubmissionEventDetail(regId: String,
                                 authProviderId: String,
                                 transId: Option[String],
                                 uprn: Option[String],
                                 addressEventType: String,
                                 jsSubmission: JsObject)

object SubmissionEventDetail {

  import RegistrationAuditEventConstants.{ACK_REF, CORP_TAX, JOURNEY_ID, REG_METADATA}

  implicit val writes: Writes[SubmissionEventDetail] = (detail: SubmissionEventDetail) => {

    def businessAddressAuditWrites(address: BusinessAddress) = BusinessAddress.auditWrites(detail.transId, detail.addressEventType, detail.uprn, address)

    def businessContactAuditWrites(contact: BusinessContactDetails) = BusinessContactDetails.auditWrites(contact)

    val address = (detail.jsSubmission \ "registration" \ "corporationTax" \ "businessAddress").
      asOpt[BusinessAddress].fold {
        Json.obj()
      } {
        address =>
          if (detail.transId.isDefined) {
            Json.obj("businessAddress" -> Json.toJson(address)(businessAddressAuditWrites(address)).as[JsObject])
          } else {
            Json.obj()
          }
      }

    val contactDetails = (detail.jsSubmission \ "registration" \ "corporationTax" \ "businessContactDetails").
      asOpt[BusinessContactDetails].fold {
        Json.obj()
      } {
        contact => Json.obj("businessContactDetails" -> Json.toJson(contact)(businessContactAuditWrites(contact)).as[JsObject])
      }

    val corporationTax = (detail.jsSubmission \ "registration" \ "corporationTax").as[JsObject] - "businessAddress"

    Json.obj(
      JOURNEY_ID -> detail.regId,
      ACK_REF -> (detail.jsSubmission \ "acknowledgementReference").as[JsString],
      REG_METADATA -> (detail.jsSubmission \ "registration" \ "metadata").as[JsObject].++(
        Json.obj("authProviderId" -> detail.authProviderId)
      ).-("sessionId").-("credentialId"),
      CORP_TAX -> corporationTax.++
      (address).++
      (contactDetails)
    ) ++ Json.obj("desSubmissionState" -> "partial")
  }
}
