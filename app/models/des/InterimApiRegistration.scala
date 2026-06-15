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

package models.des

import models.validation.APIValidation
import models.{Groups, TakeoverDetails}
import play.api.libs.functional.syntax._
import play.api.libs.json.Writes._
import play.api.libs.json._

import java.time.Instant
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}

object ApiFormats {

  private val datetime: DateTimeFormatter = new DateTimeFormatterBuilder().appendInstant(3).toFormatter

  def formatTimestamp(ts: Instant): String = datetime.format(ts)

}

object BusinessType {
  val LimitedCompany: String = "Limited company"
}

sealed trait CompletionCapacity {
  def text: String
}

object CompletionCapacity {
  implicit val writes: Writes[CompletionCapacity] = (cc: CompletionCapacity) => JsString(cc.text)

  def apply(text: String): CompletionCapacity = text.toLowerCase match {
    case d if d == Director.text.toLowerCase => Director
    case s if s == Secretary.text.toLowerCase => Secretary
    case a if a == Agent.text.toLowerCase => Agent
    case _ => Other(text)
  }
}

case object Director extends CompletionCapacity {
  val text: String = "Director"
}

case object Secretary extends CompletionCapacity {
  val text: String = "Company secretary"
}

case object Agent extends CompletionCapacity {
  val text: String = "Agent"
}

case class Other(txt: String) extends CompletionCapacity {
  val text: String = txt
}

case class BusinessAddress(line1: String,
                           line2: String,
                           line3: Option[String],
                           line4: Option[String],
                           postcode: Option[String],
                           country: Option[String])

object BusinessAddress {

  implicit val writes: Writes[BusinessAddress] = (
    (__ \ "line1").write[String] and
      (__ \ "line2").write[String] and
      (__ \ "line3").writeNullable[String] and
      (__ \ "line4").writeNullable[String] and
      (__ \ "postcode").writeNullable[String] and
      (__ \ "country").writeNullable[String]
    ) (unlift(BusinessAddress.unapply))

  implicit val reads: Reads[BusinessAddress] = (
    (__ \ "line1").read[String] and
      (__ \ "line2").read[String] and
      (__ \ "line3").readNullable[String] and
      (__ \ "line4").readNullable[String] and
      (__ \ "postcode").readNullable[String] and
      (__ \ "country").readNullable[String]
    ) (BusinessAddress.apply _)

  def auditWrites(addressTransId: Option[String], entryMethod: String, uprn: Option[String], address: BusinessAddress): Writes[BusinessAddress] = {
    val aWrites: OWrites[BusinessAddress] = (
      (__ \ "addressLine1").write[String] and
        (__ \ "addressLine2").write[String] and
        (__ \ "addressLine3").writeNullable[String] and
        (__ \ "addressLine4").writeNullable[String] and
        (__ \ "postCode").writeNullable[String] and
        (__ \ "country").writeNullable[String]
      ) (unlift(BusinessAddress.unapply))

    (businessAddress: BusinessAddress) => {
      Json.obj(
        "addressEntryMethod" -> entryMethod
      ).++(
        addressTransId.fold[JsObject](Json.obj())(id => Json.obj("transactionId" -> id))
      ).++(
        Json.toJson(address)(aWrites).as[JsObject]
      ).++(
        uprn.fold[JsObject](Json.obj())(_ => Json.obj("uprn" -> uprn))
      )
    }
  }
}

case class BusinessContactDetails(phoneNumber: Option[String],
                                  mobileNumber: Option[String],
                                  email: Option[String])

object BusinessContactDetails {
  implicit val writes: Writes[BusinessContactDetails] = (
    (__ \ "phoneNumber").writeNullable[String] and
      (__ \ "mobileNumber").writeNullable[String] and
      (__ \ "email").writeNullable[String]
    ) (unlift(BusinessContactDetails.unapply))

  implicit val reads: Reads[BusinessContactDetails] = (
    (__ \ "phoneNumber").readNullable[String] and
      (__ \ "mobileNumber").readNullable[String] and
      (__ \ "email").readNullable[String]
    ) (BusinessContactDetails.apply _)

  def auditWrites(details: BusinessContactDetails): Writes[BusinessContactDetails] = {

    val aWrites: OWrites[BusinessContactDetails] = (
      (__ \ "telephoneNumber").writeNullable[String] and
        (__ \ "mobileNumber").writeNullable[String] and
        (__ \ "emailAddress").writeNullable[String]
      ) (unlift(BusinessContactDetails.unapply))

    (businessContactDetails: BusinessContactDetails) => {
      Json.toJson(businessContactDetails)(aWrites).as[JsObject]
    }
  }
}

case class Metadata(sessionId: String,
                    credId: String,
                    language: String,
                    submissionTs: Instant,
                    completionCapacity: CompletionCapacity)

object Metadata {

  import ApiFormats._

  implicit val writes: Writes[Metadata] = (metadata: Metadata) => {
    Json.obj(
      "businessType" -> BusinessType.LimitedCompany,
      "submissionFromAgent" -> false,
      "declareAccurateAndComplete" -> true,
      "sessionId" -> metadata.sessionId,
      "credentialId" -> metadata.credId,
      "language" -> metadata.language,
      "formCreationTimestamp" -> formatTimestamp(metadata.submissionTs)
    ) ++ (
      metadata.completionCapacity match {
        case Other(cc) =>
          Json.obj(
            "completionCapacity" -> "Other",
            "completionCapacityOther" -> cc
          )
        case _ => Json.obj("completionCapacity" -> metadata.completionCapacity)
      }
      )
  }
}

case class InterimCorporationTax(companyName: String,
                                 returnsOnCT61: Boolean,
                                 businessAddress: Option[BusinessAddress],
                                 businessContactDetails: BusinessContactDetails,
                                 groups: Option[Groups] = None,
                                 takeOver: Option[TakeoverDetails] = None)

object InterimCorporationTax {

  implicit val writes: Writes[InterimCorporationTax] = (interimCorporationTax: InterimCorporationTax) => {
    val address = interimCorporationTax.businessAddress map {
      Json.toJson(_).as[JsObject]
    }

    val contactDetails: JsObject = Json.toJson(interimCorporationTax.businessContactDetails).as[JsObject]

    val groupsBlock: JsObject = interimCorporationTax.groups match {
      case Some(groups) if groups.groupRelief => Json.obj("companyMemberOfGroup" -> true,
        "groupDetails" ->
          Json.obj(
              "parentCompanyName" -> groups.nameOfCompany.get.name,
              "groupAddress" -> groups.addressAndType.get.address)
            .deepMerge(groups.groupUTR.get.UTR.fold(Json.obj())(utr => Json.obj("parentUTR" -> utr)
            )
            )
      )
      case _ => Json.obj("companyMemberOfGroup" -> false)

    }

    val takeOverBlock: JsObject = interimCorporationTax.takeOver match {
      case Some(takeOver) if takeOver.replacingAnotherBusiness => Json.obj("hasCompanyTakenOverBusiness" -> true,
        "businessTakeOverDetails" ->
          Json.obj(
            "businessNameLine1" -> takeOver.businessName.map(str =>
              APIValidation.takeoverCompanyNameInverseRegex.r.replaceAllIn(str, "")
            ),
            "businessTakeoverAddress" -> takeOver.businessTakeoverAddress,
            "prevOwnersName" -> takeOver.prevOwnersName,
            "prevOwnerAddress" -> takeOver.prevOwnersAddress
          ))
      case _ => Json.obj("hasCompanyTakenOverBusiness" -> false)
    }

    Json.obj(
      "companyOfficeNumber" -> "623").deepMerge(takeOverBlock) ++
      Json.obj(
        "companiesHouseCompanyName" -> APIValidation.cleanseCompanyName(interimCorporationTax.companyName),
        "returnsOnCT61" -> interimCorporationTax.returnsOnCT61,
        "companyACharity" -> false
      ).deepMerge(groupsBlock) ++
      address.fold(Json.obj())(add => Json.obj("businessAddress" -> add)) ++
      Json.obj("businessContactDetails" -> contactDetails)
  }

}

case class InterimApiRegistration(ackRef: String,
                                  metadata: Metadata,
                                  interimCorporationTax: InterimCorporationTax)

object InterimApiRegistration {
  implicit val writes: Writes[InterimApiRegistration] = (
    (__ \ "acknowledgementReference").write[String] and
      (__ \ "registration" \ "metadata").write[Metadata] and
      (__ \ "registration" \ "corporationTax").write[InterimCorporationTax]
    ) (unlift(InterimApiRegistration.unapply))
}
