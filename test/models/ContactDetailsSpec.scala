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

import models.validation.MongoValidation
import org.scalatestplus.play.PlaySpec
import play.api.libs.json._
import play.api.libs.json.JsonValidationError

class ContactDetailsSpec extends PlaySpec with JsonFormatValidation {

  type OS = Option[String]
  type S = String

  def lineEnd(comma: Boolean): S = if (comma) "," else ""

  def jsonLine(key: S, value: OS, comma: Boolean = true): OS = value.map(v => s""""$key" : "$v"${lineEnd(comma)}""")

  def j(p: OS = None, m: OS = None, e: OS = Some("a@b.c")): S = {
    val extra: S = Seq(
      jsonLine("contactDaytimeTelephoneNumber", p, comma = false),
      jsonLine("contactMobileNumber", m, comma = false),
      jsonLine("contactEmail", e, comma = false)
    ).flatten.mkString(", ")
    s"""
       |{
       |  $extra
       |}
     """.stripMargin
  }

  "ContactDetails Model - phone number" must {
    "build from JSON" when {
      "valid phone numbers are supplied" in {
        val json = j(p = Some("1234567890"), m = Some("1234567890"), e = None)
        val expected = ContactDetails(Some("1234567890"), Some("1234567890"), None)
        val result = Json.parse(json).validate[ContactDetails]
        mustBeSuccess(expected, result)
      }
      "valid phone numbers with spaces are supplied" in {
        val json = j(p = Some("12345   67890"), m = Some("1234567890"), e = None)
        val expected = ContactDetails(Some("12345   67890"), Some("1234567890"), None)
        val result = Json.parse(json).validate[ContactDetails]
        mustBeSuccess(expected, result)
      }
      "invalid phone number is read from mongo" in {
        val json = j(p = Some("12345"), m = Some("1234567890"), e = None)
        val expected = ContactDetails(Some("12345"), Some("1234567890"), None)
        val result = Json.parse(json).validate[ContactDetails](ContactDetails.format(MongoValidation))
        mustBeSuccess(expected, result)
      }
    }
    "fail from JSON" when {
      "invalid phone numbers are supplied" in {
        val json = j(p = Some("123456"), m = Some("1234567890"))
        val result = Json.parse(json).validate[ContactDetails]
        shouldHaveErrors(result, JsPath() \ "contactDaytimeTelephoneNumber", Seq(JsonValidationError("field must contain between 10 and 20 numbers")))
      }
      "invalid phone numbers are supplied with spaces" in {
        val json = j(p = Some("1234567890"), m = Some("12345   678"))
        val result = Json.parse(json).validate[ContactDetails]
        shouldHaveErrors(result, JsPath() \ "contactMobileNumber", Seq(JsonValidationError("field must contain between 10 and 20 numbers")))
      }
    }
  }

  "ContactDetails Model - one detail supplied" must {

    "build from JSON - phone" in {
      val json = j(p = Some("1234567890"), e = None)
      val expected = ContactDetails(Some("1234567890"), None, None)
      val result = Json.parse(json).validate[ContactDetails]
      mustBeSuccess(expected, result)
    }
    "build from JSON - mobile" in {
      val json = j(m = Some("1234567890"), e = None)
      val expected = ContactDetails(None, Some("1234567890"), None)
      val result = Json.parse(json).validate[ContactDetails]
      mustBeSuccess(expected, result)
    }

    "fail if email is invalid" ignore { // ignored for now due to regex clarification!
      val json = j(e = Some("testInvalidEmail"))
      val result = Json.parse(json).validate[ContactDetails]
      shouldHaveErrors(result, JsPath() \ "contactEmail", Seq(JsonValidationError("error.pattern")))
    }

    "check with a valid email address" in {
      val contactDetails = ContactDetails(phone = None, mobile = None, email = Some("xxx@xxx.com"))
      val jsonNoContact = Json.parse("""{"contactEmail":"xxx@xxx.com"}""")

      val result = Json.fromJson[ContactDetails](jsonNoContact)
      result mustBe JsSuccess(contactDetails)
    }

    "check with a valid email address containing a hyphen" in {
      val contactDetails = ContactDetails(None, Some("1234567890"), Some("xxx@xxx-xxx.com"))
      val json = """{"contactMobileNumber":"1234567890", "contactEmail":"xxx@xxx-xxx.com"}"""
      val result = Json.parse(json).validate[ContactDetails]

      result mustBe JsSuccess(contactDetails)
    }

    "check with an email address containing a plus - that the API can't accept" in {
      val json = """{"contactMobileNumber":"1234567890", "contactEmail":"xxx+xxx@xxx.com"}"""
      val result = Json.parse(json).validate[ContactDetails]

      shouldHaveErrors(result, JsPath() \ "contactEmail", Seq(JsonValidationError("error.pattern")))
    }
  }

  "reading from json into a ContactDetails case class" must {

    "return a ContactDetails case class" in {
      val contactDetails = ContactDetails(None, Some("1234567890"), None)
      val jsonNoContact = Json.parse("""{"contactMobileNumber":"1234567890"}""")

      val result = Json.fromJson[ContactDetails](jsonNoContact)
      result mustBe JsSuccess(contactDetails)
    }

    "throw a json validation error" in {
      val jsonNoContact = Json.parse("""{}""")

      val result = Json.fromJson[ContactDetails](jsonNoContact)
      shouldHaveErrors(result, JsPath(), Seq(JsonValidationError("Must have at least one email, phone or mobile specified")))
    }
  }
}