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

package models

import auth.CryptoSCRS
import models.api.BusinessAddress
import models.validation.{APIValidation, BaseJsonFormatting}
import play.api.libs.json.Format
import play.api.libs.functional.syntax._
import play.api.libs.json._

object GroupCompanyNameEnum extends Enumeration {
  val Other: GroupCompanyNameEnum.Value = Value
  val CohoEntered: GroupCompanyNameEnum.Value = Value
}

case class GroupCompanyName(name: String, nameType: GroupCompanyNameEnum.Value)

case class GroupUTR(UTR: Option[String])

object GroupUTR {
  def formats(baseJsonFormatting: BaseJsonFormatting, cryptoSCRS: CryptoSCRS): Format[GroupUTR] = new Format[GroupUTR] {

    override def reads(json: JsValue): JsResult[GroupUTR] = {
      val utr = (json \ "UTR").validateOpt[String](baseJsonFormatting.utrFormats(cryptoSCRS))
      utr.map(optUtr => GroupUTR(optUtr))
    }

    override def writes(o: GroupUTR): JsValue = o.UTR.fold(Json.obj())(utr
    => Json.obj("UTR" -> baseJsonFormatting.utrFormats(cryptoSCRS).writes(utr)))
  }
}

object GroupCompanyName {
  def formats(baseJsonFormatting: BaseJsonFormatting): Format[GroupCompanyName] = new Format[GroupCompanyName] {
    override def reads(json: JsValue): JsResult[GroupCompanyName] = {
      for {
        name <- (json \ "name").validate[String](baseJsonFormatting.groupNameValidation)
        nameType <- (json \ "nameType").validate[GroupCompanyNameEnum.Value](baseJsonFormatting.formatsForGroupCompanyNameEnum(name))
      } yield GroupCompanyName(name, nameType)
    }

    override def writes(o: GroupCompanyName): JsValue = Json.obj("name" -> o.name, "nameType" -> o.nameType.toString)
  }
}

object GroupAddressTypeEnum extends Enumeration {
  val ALF: GroupAddressTypeEnum.Value = Value
  val CohoEntered: GroupAddressTypeEnum.Value = Value
}

case class GroupsAddressAndType(addressType: GroupAddressTypeEnum.Value, address: BusinessAddress)

object GroupsAddressAndTypeFormats {
  def bAddressformats(formatter: BaseJsonFormatting): Format[BusinessAddress] = (
    (__ \ "line1").format[String](formatter.lineValidator) and
      (__ \ "line2").format[String](formatter.lineValidator) and
      (__ \ "line3").formatNullable[String](formatter.lineValidator) and
      (__ \ "line4").formatNullable[String](formatter.line4Validator) and
      (__ \ "postcode").formatNullable[String](formatter.postcodeValidator) and
      (__ \ "country").formatNullable[String](formatter.countryValidator)
    ) (BusinessAddress.apply, unlift(BusinessAddress.unapply))

  def groupsAddressAndTypeFormats(formatter: BaseJsonFormatting): Format[GroupsAddressAndType] = (
    (__ \ "addressType").format[GroupAddressTypeEnum.Value](formatter.formatsForGroupAddressType) and
      (__ \ "address").format[BusinessAddress](bAddressformats(formatter))
    ) (GroupsAddressAndType.apply, unlift(GroupsAddressAndType.unapply))
}

case class Groups(
                   groupRelief: Boolean,
                   nameOfCompany: Option[GroupCompanyName],
                   addressAndType: Option[GroupsAddressAndType],
                   groupUTR: Option[GroupUTR]
                 )

object Groups {
  def formats(baseJsonFormatting: BaseJsonFormatting, cryptoSCRS: CryptoSCRS): Format[Groups] = (
    (__ \ "groupRelief").format[Boolean] and
      (__ \ "nameOfCompany").formatNullable[GroupCompanyName](GroupCompanyName.formats(baseJsonFormatting)) and
      (__ \ "addressAndType").formatNullable[GroupsAddressAndType](GroupsAddressAndTypeFormats.groupsAddressAndTypeFormats(baseJsonFormatting)) and
      (__ \ "groupUTR").formatNullable[GroupUTR](GroupUTR.formats(baseJsonFormatting, cryptoSCRS))
    ) (Groups.apply, unlift(Groups.unapply))
}

object GroupNameListValidator {
  val formats: Format[Seq[String]] = new Format[Seq[String]] {
    override def reads(json: JsValue): JsResult[Seq[String]] = {
      val seq = json.validate[JsArray].map(js => js.value.toSeq)
      seq.map(_.collect {
        case jsVal if jsVal.validate[String](APIValidation.parentGroupNameValidator).isSuccess => jsVal.as[String]
      })

    }

    override def writes(o: Seq[String]): JsValue = Json.toJson(o)
  }
}