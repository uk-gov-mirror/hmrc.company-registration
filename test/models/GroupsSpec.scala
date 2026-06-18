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

import helpers.BaseSpec
import models.api.BusinessAddress
import models.validation.{APIValidation, MongoValidation}
import play.api.libs.json.{Format, JsValue, Json}
import utils.LogCapturingHelper

class GroupsSpec extends BaseSpec with LogCapturingHelper {

  val formatsOfGroupsAPI: Format[Groups] = Groups.formats(APIValidation, mockInstanceOfCrypto)
  val formatsOfGroupsMongo: Format[Groups] = Groups.formats(MongoValidation, mockInstanceOfCrypto)

  val validGroupsModel: Groups = Groups(
    groupRelief = true,
    nameOfCompany = Some(GroupCompanyName("testGroupName", GroupCompanyNameEnum.Other)),
    addressAndType = Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF, BusinessAddress("1 abc", "2 abc", Some("3 abc"), Some("4 abc"), Some("ZZ1 1ZZ"), Some("country A")))),
    groupUTR = Some(GroupUTR(Some("1234567890"))))

  val encryptedUTR: JsValue = mockInstanceOfCrypto.wts.writes("1234567890")

  val fullGroupJson: JsValue = Json.parse(
    """
      |{
      |   "groupRelief": true,
      |   "nameOfCompany": {
      |     "name": "testGroupName",
      |     "nameType" : "Other"
      |   },
      |   "addressAndType" : {
      |     "addressType" : "ALF",
      |       "address" : {
      |         "line1": "1 abc",
      |         "line2" : "2 abc",
      |         "line3" : "3 abc",
      |         "line4" : "4 abc",
      |         "country" : "country A",
      |         "postcode" : "ZZ1 1ZZ"
      |     }
      |   },
      |   "groupUTR" : {
      |     "UTR" : "1234567890"
      |   }
      |}
    """.stripMargin)

  "API Reads" must {
    "read in full json object" in {
      fullGroupJson.as[Groups](formatsOfGroupsAPI) mustBe validGroupsModel
    }
    "read in json with minimum data" in {
      val minimumJson = Json.parse(
        """ {"groupRelief": true }""".stripMargin)

      minimumJson.as[Groups](formatsOfGroupsAPI) mustBe Groups(groupRelief = true, None, None, None)
    }
    "read in json with valid data over maximum lengths for all address fields, truncates successfully" in {
      val fullGroupJsonEmptyUTRMaxLengthAddress = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": "testGroupName",
          |     "nameType" : "Other"
          |   },
          |   "addressAndType" : {
          |     "addressType" : "ALF",
          |       "address" : {
          |         "line1": "123 abc def ghi jkl mnop288d",
          |         "line2" : "123 abc def ghi jkl mnop288d",
          |         "line3" : "123 abc def ghi jkl mnop288d",
          |         "line4" : "abc def 123 abcd199",
          |         "country" : "21 chars not abcd 201",
          |         "postcode" : "ZZ1 1ZZ"
          |     }
          |   },
          |   "groupUTR" : {
          |
          |   }
          |}
        """.stripMargin)
      val res = fullGroupJsonEmptyUTRMaxLengthAddress.as[Groups](formatsOfGroupsAPI)
      res mustBe validGroupsModel.copy(
        addressAndType = Some(
          GroupsAddressAndType(
            GroupAddressTypeEnum.ALF,
            BusinessAddress(
              "123 abc def ghi jkl mnop288",
              "123 abc def ghi jkl mnop288",
              Some("123 abc def ghi jkl mnop288"),
              Some("abc def 123 abcd19"),
              Some("ZZ1 1ZZ"),
              Some("21 chars not abcd 20"))
          )),
        groupUTR = Some(GroupUTR(None)))
    }
    "read in json with normalisable data for address fields, return normalised data" in {
      val fullGroupJsonEmptyUTRNormalisableAddress = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": "testGroupName",
          |     "nameType" : "Other"
          |   },
          |   "addressAndType" : {
          |     "addressType" : "ALF",
          |       "address" : {
          |         "line1": "Æø",
          |         "line2" : "Æø",
          |         "line3" : "Æø",
          |         "line4" : "Æø",
          |         "country" : "Æø",
          |         "postcode" : "ZZ1 1Æ"
          |     }
          |   },
          |   "groupUTR" : {
          |
          |   }
          |}
        """.stripMargin)
      val res = fullGroupJsonEmptyUTRNormalisableAddress.as[Groups](formatsOfGroupsAPI)
      res mustBe validGroupsModel.copy(
        addressAndType = Some(
          GroupsAddressAndType(
            GroupAddressTypeEnum.ALF,
            BusinessAddress(
              "AEo",
              "AEo",
              Some("AEo"),
              Some("AEo"),
              Some("ZZ1 1AE"),
              Some("AEo"))
          )),
        groupUTR = Some(GroupUTR(None)))
    }
    "read in json with name that is longer than 20 chars but can be normalised and trimmed so validation passed, no log thrown" in {
      val groupJsonNameOfCompanyValidButLong = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": "This is longerÆ than 20 characters but thats fine because it can be normalised and trimmed to 20 chars and matches gateway regex",
          |     "nameType" : "CohoEntered"
          |   }
          |}
        """.stripMargin)
      withCaptureOfLoggingFrom(APIValidation.logger) { logEvents =>

        val expectedMessage = s"[APIValidation][Groups API Reads] name of company invalid when normalised and trimmed this doesn't pass Regex validation"
        logEvents.map(events => (events.getLevel.toString, events.getMessage)).contains(("WARN", expectedMessage)) mustBe false

        val res = groupJsonNameOfCompanyValidButLong.as[Groups](formatsOfGroupsAPI)
        res mustBe Groups(
          groupRelief = true,
          nameOfCompany = Some(GroupCompanyName(
            "This is longerÆ than 20 characters but thats fine because it can be normalised and trimmed to 20 chars and matches gateway regex",
            GroupCompanyNameEnum.CohoEntered)),
          None,
          None)
      }
    }

    "read in json with empty utr field indicating that user has not entered one but submitted on the page" in {
      val fullGroupJsonEmptyUTR = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": "testGroupName",
          |     "nameType" : "Other"
          |   },
          |   "addressAndType" : {
          |     "addressType" : "ALF",
          |       "address" : {
          |         "line1": "1 abc",
          |         "line2" : "2 abc",
          |         "line3" : "3 abc",
          |         "line4" : "4 abc",
          |         "country" : "country A",
          |         "postcode" : "ZZ1 1ZZ"
          |     }
          |   },
          |   "groupUTR" : {
          |
          |   }
          |}
        """.stripMargin)
      fullGroupJsonEmptyUTR.as[Groups](formatsOfGroupsAPI) mustBe validGroupsModel.copy(groupUTR = Some(GroupUTR(None)))
    }
    s"allow json reads to read nameOfCompany > 20 chars but log as a warn for ${GroupCompanyNameEnum.Other}" in {
      val json = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": "123456789012345678901",
          |     "nameType" : "Other"
          |   }
         }
        """.stripMargin)
      withCaptureOfLoggingFrom(APIValidation.logger) { logEvents =>
        json.as[Groups](formatsOfGroupsAPI) mustBe Groups(groupRelief = true, Some(GroupCompanyName("123456789012345678901", GroupCompanyNameEnum.Other)), None, None)
        val expectedMessage = s"[APIValidation][Groups API Reads] nameOfCompany.nameType = ${GroupCompanyNameEnum.Other} but name.size > 20, could indicate frontend validation issue"
        logEvents.map(events => (events.getLevel.toString, events.getMessage)).contains(("WARN", expectedMessage)) mustBe true

      }
    }
    s"allow json reads to read nameOfCompany > 20 chars but dont log for ${GroupCompanyNameEnum.CohoEntered}" in {
      val json = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": "123456789012345678901",
          |     "nameType" : "CohoEntered"
          |   }
         }
        """.stripMargin)
      withCaptureOfLoggingFrom(MongoValidation.logger) { logEvents =>
        json.as[Groups](formatsOfGroupsAPI) mustBe Groups(groupRelief = true, Some(GroupCompanyName("123456789012345678901", GroupCompanyNameEnum.CohoEntered)), None, None)
        val expectedMessage = s"[APIValidation][Groups API Reads] nameOfCompany.nameType = ${GroupCompanyNameEnum.Other} but name.size > 20, could indicate frontend validation issue"
        logEvents.map(events => (events.getLevel.toString, events.getMessage)).contains(("WARN", expectedMessage)) mustBe false
      }
    }
    "read if UTR is 1 number" in {
      val utr1NumberInJson = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "groupUTR" : {
          |     "UTR" : "0"
          |   }
          |}
        """.stripMargin)
      utr1NumberInJson.validate[Groups](formatsOfGroupsAPI).isSuccess mustBe true

    }

    "NOT read json if UTR is over 10 numbers" in {
      val utrJSon11Chars = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "groupUTR" : {
          |     "UTR" : "12345678901"
          |   }
          |}
        """.stripMargin)

      val message = "UTR does not match regex"
      val resToBeSure = utrJSon11Chars.validate[Groups](formatsOfGroupsAPI).fold[Boolean](
        error => error.head._2.head.message == message, _ => false)
      resToBeSure mustBe true
    }
    "NOT read json if UTR is < 1 numbers" in {
      val jsonNoChars = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "groupUTR" : {
          |     "UTR" : ""
          |   }
          |}
        """.stripMargin)

      val message = "UTR does not match regex"
      val resToBeSure = jsonNoChars.validate[Groups](formatsOfGroupsAPI).fold[Boolean](
        error => error.head._2.head.message == message, _ => false)
      resToBeSure mustBe true
    }
    "NOT read json if UTR is is chars and numbers " in {
      val jsonMixOfChars = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "groupUTR" : {
          |     "UTR" : "abc 123"
          |   }
          |}
        """.stripMargin)

      val message = "UTR does not match regex"
      val resToBeSure = jsonMixOfChars.validate[Groups](formatsOfGroupsAPI).fold[Boolean](
        error => error.head._2.head.message == message, _ => false)
      resToBeSure mustBe true
    }
    "NOT read json if name is > 20 Chars and it does not pass the gateway regex after normalisation and trim, also log a message as cohos validation is invalid" in {
      val groupJsonNameOfCompanyInvalidFor20chars = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": "&^*%&^*%&^*%&^*%&^*%&^*%&^*%&^*%&^*%&^*%&^*%&^*%&^*%",
          |     "nameType" : "CohoEntered"
          |   }
          |}
        """.stripMargin)

      withCaptureOfLoggingFrom(APIValidation.logger) { logEvents =>
        groupJsonNameOfCompanyInvalidFor20chars.validate[Groups](formatsOfGroupsAPI).isError mustBe true
        val expectedMessage = s"[APIValidation][Groups API Reads] name of company invalid when normalised and trimmed this doesn't pass Regex validation"
        logEvents.map(events => (events.getLevel.toString, events.getMessage)).contains(("WARN", expectedMessage)) mustBe true
      }
    }
    "NOT read json if name is ''" in {
      val groupJsonNameOfCompanyInvalidFor20chars = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": "",
          |     "nameType" : "CohoEntered"
          |   }
          |}
        """.stripMargin)

      withCaptureOfLoggingFrom(APIValidation.logger) { logEvents =>
        groupJsonNameOfCompanyInvalidFor20chars.validate[Groups](formatsOfGroupsAPI).isError mustBe true
        val expectedMessage = s"[APIValidation][Groups API Reads] name of company invalid when normalised and trimmed this doesn't pass Regex validation"
        logEvents.map(events => (events.getLevel.toString, events.getMessage)).contains(("WARN", expectedMessage)) mustBe true
      }
    }
    "NOT read json if name is ' '" in {
      val groupJsonNameOfCompanyInvalidFor20chars = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": " ",
          |     "nameType" : "CohoEntered"
          |   }
          |}
        """.stripMargin)

      withCaptureOfLoggingFrom(APIValidation.logger) { logEvents =>
        groupJsonNameOfCompanyInvalidFor20chars.validate[Groups](formatsOfGroupsAPI).isError mustBe true
        val expectedMessage = s"[APIValidation][Groups API Reads] name of company invalid when normalised and trimmed this doesn't pass Regex validation"
        logEvents.map(events => (events.getLevel.toString, events.getMessage)).contains(("WARN", expectedMessage)) mustBe true
      }
    }
    "NOT read json if nameType is not an enum" in {
      val nameTypeWrongEnum = Json.parse(
        """
          |{"groupRelief": true,
          |    "nameOfCompany": {
          |     "name": "testGroupName",
          |     "nameType" : "TEST"
          |   }
          | }""".stripMargin)
      val message = "String value is not an enum: TEST"
      val resToBeSure = nameTypeWrongEnum.validate[Groups](formatsOfGroupsAPI).fold[Boolean](
        error => error.head._2.head.message == message, _ => false
      )
      resToBeSure mustBe true
    }
    "NOT read json if addressType is not an enum" in {
      val incorrectAddressType = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": "testGroupName",
          |     "nameType" : "Other"
          |   },
          |   "addressAndType" : {
          |     "addressType" : "TEST",
          |       "address" : {
          |         "line1": "1 abc",
          |         "line2" : "2 abc",
          |         "line3" : "3 abc",
          |         "line4" : "4 abc",
          |         "country" : "country A",
          |         "postcode" : "ZZ1 1ZZ"
          |     }
          |   }
          |}
        """.stripMargin)
      val message = "String value is not an enum: TEST"
      val resToBeSure = incorrectAddressType.validate[Groups](formatsOfGroupsAPI).fold[Boolean](
        error => error.head._2.head.message == message, _ => false
      )
      resToBeSure mustBe true
    }
  }
  "API Writes" must {
    "write Groups to json" in {
      Json.toJson[Groups](validGroupsModel)(formatsOfGroupsAPI) mustBe fullGroupJson
    }
    "write mimimum data in groups model to json" in {
      Json.toJson[Groups](Groups(groupRelief = false, None, None, None))(formatsOfGroupsAPI) mustBe Json.obj("groupRelief" -> false)
    }
    "write group utr not entered to json" in {
      val expectedFullGroupJsonEmptyUTR = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": "testGroupName",
          |     "nameType" : "Other"
          |   },
          |   "addressAndType" : {
          |     "addressType" : "ALF",
          |       "address" : {
          |         "line1": "1 abc",
          |         "line2" : "2 abc",
          |         "line3" : "3 abc",
          |         "line4" : "4 abc",
          |         "country" : "country A",
          |         "postcode" : "ZZ1 1ZZ"
          |     }
          |   },
          |   "groupUTR" : {
          |
          |   }
          |}
        """.stripMargin)
      Json.toJson[Groups](validGroupsModel.copy(groupUTR = Some(GroupUTR(None))))(formatsOfGroupsAPI) mustBe expectedFullGroupJsonEmptyUTR
    }
  }
  "Mongo reads" must {
    "read full json containing encrypted utr into case class" in {

      val fullGroupJsonEncryptedUTR = Json.parse(
        s"""
           |{
           |   "groupRelief": true,
           |   "nameOfCompany": {
           |     "name": "testGroupName",
           |     "nameType" : "Other"
           |   },
           |   "addressAndType" : {
           |     "addressType" : "ALF",
           |       "address" : {
           |         "line1": "1 abc",
           |         "line2" : "2 abc",
           |         "line3" : "3 abc",
           |         "line4" : "4 abc",
           |         "country" : "country A",
           |         "postcode" : "ZZ1 1ZZ"
           |     }
           |   },
           |   "groupUTR" : {
           |     "UTR" : $encryptedUTR
           |   }
           |}
        """.stripMargin)
      fullGroupJsonEncryptedUTR.as[Groups](formatsOfGroupsMongo) mustBe validGroupsModel
    }
    "convert nameType To 'Other' if nameType is not an enum in db, and log this as a warn" in {
      val json = Json.parse(
        s"""
           |{
           |   "groupRelief": true,
           |   "nameOfCompany": {
           |     "name": "testGroupName",
           |     "nameType" : "TEST"
           |   }
           |}
          """.stripMargin)
      withCaptureOfLoggingFrom(MongoValidation.logger) { logEvents =>
        json.as[Groups](formatsOfGroupsMongo) mustBe Groups(groupRelief = true, Some(GroupCompanyName("testGroupName", GroupCompanyNameEnum.Other)), None, None)
        val expectedMessage = "[MongoValidation][Groups Mongo Reads] nameOfCompany.nameType was: TEST, converted to Other"
        logEvents.map(events => (events.getLevel.toString, events.getMessage)).contains(("WARN", expectedMessage)) mustBe true
      }
    }

    "read address over max length without trimming" in {
      val fullGroupJsonEmptyUTRMaxLengthAddress = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": "testGroupName",
          |     "nameType" : "Other"
          |   },
          |   "addressAndType" : {
          |     "addressType" : "CohoEntered",
          |       "address" : {
          |         "line1": "123 abc def ghi jkl mnop288d",
          |         "line2" : "123 abc def ghi jkl mnop288d",
          |         "line3" : "123 abc def ghi jkl mnop288d",
          |         "line4" : "abc def 123 abcd199",
          |         "country" : "21 chars not abcd 201",
          |         "postcode" : "ZZ1 1ZZ"
          |     }
          |   },
          |   "groupUTR" : {
          |
          |   }
          |}
        """.stripMargin)
      val res = fullGroupJsonEmptyUTRMaxLengthAddress.as[Groups](formatsOfGroupsMongo)
      res mustBe validGroupsModel.copy(
        addressAndType = Some(
          GroupsAddressAndType(
            GroupAddressTypeEnum.CohoEntered,
            BusinessAddress(
              "123 abc def ghi jkl mnop288d",
              "123 abc def ghi jkl mnop288d",
              Some("123 abc def ghi jkl mnop288d"),
              Some("abc def 123 abcd199"),
              Some("ZZ1 1ZZ"),
              Some("21 chars not abcd 201"))
          )),
        groupUTR = Some(GroupUTR(None)))
    }
    "convert addressType to ALF if addressType is not an enum in db, and log this as a warn" in {
      val jsonUhOh = Json.parse(
        s"""
           |{
           |   "groupRelief": true,
           |   "nameOfCompany": {
           |     "name": "testGroupName",
           |     "nameType" : "Other"
           |   },
           |   "addressAndType" : {
           |     "addressType" : "Uh Oh",
           |       "address" : {
           |         "line1": "1 abc",
           |         "line2" : "2 abc",
           |         "line3" : "3 abc",
           |         "line4" : "4 abc",
           |         "country" : "country A",
           |         "postcode" : "ZZ1 1ZZ"
           |     }
           |   }
           |}
        """.stripMargin)
      withCaptureOfLoggingFrom(MongoValidation.logger) { logEvents =>
        jsonUhOh.as[Groups](formatsOfGroupsMongo) mustBe Groups(
          groupRelief = true,
          Some(GroupCompanyName("testGroupName", GroupCompanyNameEnum.Other)),
          Some(GroupsAddressAndType(
            GroupAddressTypeEnum.ALF,
            BusinessAddress("1 abc", "2 abc", Some("3 abc"), Some("4 abc"), Some("ZZ1 1ZZ"), Some("country A")))),
          None)
        val expectedMessage = "[MongoValidation][Groups Mongo Reads] addressType was: Uh Oh, converted to ALF"
        logEvents.map(events => (events.getLevel.toString, events.getMessage)).contains(("WARN", expectedMessage)) mustBe true
      }
    }
    "read invalid chars successfully" in {
      val encryptedUTRInvalidButCanBeRead = mockInstanceOfCrypto.wts.writes("1234567890ABC TOO MANY CHARS AND NOT JUST NUM")
      val fullGroupJsonEncrypted = Json.parse(
        s"""
           |{
           |   "groupRelief": true,
           |   "nameOfCompany": {
           |     "name": "Æ",
           |     "nameType" : "Other"
           |   },
           |   "addressAndType" : {
           |     "addressType" : "ALF",
           |       "address" : {
           |         "line1": "Æ",
           |         "line2" : "Æ",
           |         "line3" : "Æ",
           |         "line4" : "Æ",
           |         "country" : "",
           |         "postcode" : "Æ"
           |     }
           |   },
           |   "groupUTR" : {
           |     "UTR" : $encryptedUTRInvalidButCanBeRead
           |   }
           |}
        """.stripMargin)
      val res = fullGroupJsonEncrypted.as[Groups](formatsOfGroupsMongo)
      res mustBe Groups(
        groupRelief = true,
        nameOfCompany = Some(GroupCompanyName(
          "Æ", GroupCompanyNameEnum.Other
        )),
        addressAndType = Some(GroupsAddressAndType(
          GroupAddressTypeEnum.ALF,
          BusinessAddress("Æ", "Æ", Some("Æ"), Some("Æ"), Some("Æ"), Some("")))),
        groupUTR = Some(GroupUTR(Some("1234567890ABC TOO MANY CHARS AND NOT JUST NUM")))
      )
    }
  }
  "Mongo Writes" must {
    "write full model to json encrypting the UTR" in {
      val fullGroupJsonEncrypted = Json.parse(
        s"""
           |{
           |   "groupRelief": true,
           |   "nameOfCompany": {
           |     "name": "testGroupName",
           |     "nameType" : "Other"
           |   },
           |   "addressAndType" : {
           |     "addressType" : "ALF",
           |       "address" : {
           |         "line1": "1 abc",
           |         "line2" : "2 abc",
           |         "line3" : "3 abc",
           |         "line4" : "4 abc",
           |         "country" : "country A",
           |         "postcode" : "ZZ1 1ZZ"
           |     }
           |   },
           |   "groupUTR" : {
           |     "UTR" : $encryptedUTR
           |   }
           |}
        """.stripMargin)
      Json.toJson[Groups](validGroupsModel)(formatsOfGroupsMongo) mustBe fullGroupJsonEncrypted
    }
    "write minimum model to json" in {
      val expected = Json.obj("groupRelief" -> true)
      Json.toJson[Groups](Groups(groupRelief = true, None, None, None))(formatsOfGroupsMongo) mustBe expected
    }
    "write utr block with empty utr correctly" in {

      val expectedFullGroupJsonEmptyUTR = Json.parse(
        """
          |{
          |   "groupRelief": true,
          |   "nameOfCompany": {
          |     "name": "testGroupName",
          |     "nameType" : "Other"
          |   },
          |   "addressAndType" : {
          |     "addressType" : "ALF",
          |       "address" : {
          |         "line1": "1 abc",
          |         "line2" : "2 abc",
          |         "line3" : "3 abc",
          |         "line4" : "4 abc",
          |         "country" : "country A",
          |         "postcode" : "ZZ1 1ZZ"
          |     }
          |   },
          |   "groupUTR" : {
          |
          |   }
          |}
        """.stripMargin)
      Json.toJson[Groups](validGroupsModel.copy(groupUTR = Some(GroupUTR(None))))(formatsOfGroupsMongo) mustBe expectedFullGroupJsonEmptyUTR
    }
  }
  "GroupNameListValidator reads" must {
    "successfully read a list of names that are all valid" in {
      val jsonToTest = Json.parse(
        """
          |[
          |   "testName1","testName2","testName3","testName4"
          |]
        """.stripMargin)

      val res = jsonToTest.as[Seq[String]](GroupNameListValidator.formats)
      res mustBe Seq("testName1", "testName2", "testName3", "testName4")
    }
    "successfully filter out a name that is not valid" in {
      val jsonToTest = Json.parse(
        """
          |[
          |   "testName1", "$" ,"testName3","testName4"
          |]
        """.stripMargin)

      val res = jsonToTest.as[Seq[String]](GroupNameListValidator.formats)
      res mustBe Seq("testName1", "testName3", "testName4")
    }
    "successfully return nothing when no names are valid" in {
      val jsonToTest = Json.parse(
        """
          |[
          |   "$","$","$","$"
          |]
        """.stripMargin)

      val res = jsonToTest.as[Seq[String]](GroupNameListValidator.formats)
      res mustBe Seq.empty
    }
    "successfully return original name when normalisation can occur" in {
      val jsonToTest = Json.parse(
        """
          |[
          |   "Æ","Æ","testName3","testName4"
          |]
        """.stripMargin)
      val res = jsonToTest.as[Seq[String]](GroupNameListValidator.formats)
      res mustBe Seq("Æ", "Æ", "testName3", "testName4")
    }
    "successfully return a name over the maximum char limit because this can be trimmed after on api submission" in {
      val jsonToTest = Json.parse(
        """
          |[
          |   "abcdefg hijklmnop qrs tuv wxyz abcdefg hijklmnop qrs tuv wxyz","testName2"
          |]
        """.stripMargin)
      val res = jsonToTest.as[Seq[String]](GroupNameListValidator.formats)
      res mustBe Seq("abcdefg hijklmnop qrs tuv wxyz abcdefg hijklmnop qrs tuv wxyz", "testName2")
    }
    "can handle non strings in array, these are filtered out" in {
      val jsonToTest = Json.parse(
        """
          |[
          |1, "testName2", true
          |]
        """.stripMargin)
      val res = jsonToTest.as[Seq[String]](GroupNameListValidator.formats)
      res mustBe Seq("testName2")
    }
    "can handle an empty array" in {
      val jsonToTest = Json.parse(
        """
          |[
          |]
        """.stripMargin)
      val res = jsonToTest.as[Seq[String]](GroupNameListValidator.formats)
      res mustBe Seq.empty
    }
    "can handle an array with one empty string in" in {
      val jsonToTest = Json.parse(
        """
          |[
          |""
          |]
        """.stripMargin)
      val res = jsonToTest.as[Seq[String]](GroupNameListValidator.formats)
      res mustBe Seq.empty
    }
  }
  "GroupNameListValidator writes" must {
    "write an empty seq to array" in {
      Json.toJson(Seq.empty[String])(GroupNameListValidator.formats) mustBe Json.parse(
        """
          |[
          |]
        """.stripMargin)
    }
    "write a seq of strings to array" in {
      Json.toJson(Seq("testName1", "testName2", "testName3", "testName4"))(GroupNameListValidator.formats) mustBe Json.parse(
        """
          |[
          |"testName1","testName2","testName3","testName4"
          |]
        """.stripMargin)
    }
  }
}