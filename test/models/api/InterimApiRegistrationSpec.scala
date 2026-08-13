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

package models.api

import models._
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsObject, Json}

import java.time.Instant


class InterimApiRegistrationSpec extends PlaySpec {

  "CompletionCapacity" must {
    "Construct a director" in { CompletionCapacity("Director") mustBe Director }
    "Construct an agent" in { CompletionCapacity("Agent") mustBe Agent }
    "Construct a secretary" in { CompletionCapacity("Company secretary") mustBe Secretary }
    "Construct a direct from an other" in { CompletionCapacity("director") mustBe Director }
    "Construct an agent from an other" in { CompletionCapacity("agent") mustBe Agent }
    "Construct a secretary from an other" in { CompletionCapacity("company secretary") mustBe Secretary }
    "Construct an other" in { CompletionCapacity("other") mustBe Other("other") }
  }

  "Registration metadata model" must {

    "Simple model should produce valid JSON for a director" in {
      val expectedJson : String = s"""{
                               |  "businessType" : "Limited company",
                               |  "sessionId" : "session-123",
                               |  "credentialId" : "cred-123",
                               |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                               |  "submissionFromAgent": false,
                               |  "language" : "ENG",
                               |  "completionCapacity" : "Director",
                               |  "declareAccurateAndComplete": true
                               |}""".stripMargin

      val metadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), CompletionCapacity(Director.text) )

      val result = Json.toJson[Metadata](metadata)
      result.getClass mustBe classOf[JsObject]
      result mustBe Json.parse(expectedJson)
    }

    "Simple model should produce valid JSON for an agent" in {
      val expectedJson : String = s"""{
                               |  "businessType" : "Limited company",
                               |  "sessionId" : "session-123",
                               |  "credentialId" : "cred-123",
                               |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                               |  "submissionFromAgent": false,
                               |  "language" : "ENG",
                               |  "completionCapacity" : "Agent",
                               |  "declareAccurateAndComplete": true
                               |}""".stripMargin

      val metadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), CompletionCapacity(Agent.text) )

      val result = Json.toJson[Metadata](metadata)
      result.getClass mustBe classOf[JsObject]
      result mustBe Json.parse(expectedJson)
    }

    "Simple model should produce valid JSON for an secretary" in {
      val expectedJson : String = s"""{
                                     |  "businessType" : "Limited company",
                                     |  "sessionId" : "session-123",
                                     |  "credentialId" : "cred-123",
                                     |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                                     |  "submissionFromAgent": false,
                                     |  "language" : "ENG",
                                     |  "completionCapacity" : "Company secretary",
                                     |  "declareAccurateAndComplete": true
                                     |}""".stripMargin

      val metadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), CompletionCapacity(Secretary.text) )

      val result = Json.toJson[Metadata](metadata)
      result.getClass mustBe classOf[JsObject]
      result mustBe Json.parse(expectedJson)
    }

    "Unexpected completion capacity should produce the other fields" in {
      val expectedJson : String = s"""{
                                      |  "businessType" : "Limited company",
                                      |  "sessionId" : "session-123",
                                      |  "credentialId" : "cred-123",
                                      |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                                      |  "submissionFromAgent": false,
                                      |  "language" : "ENG",
                                      |  "completionCapacity" : "Other",
                                      |  "completionCapacityOther" : "other",
                                      |  "declareAccurateAndComplete": true
                                      |}""".stripMargin

      val metadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), CompletionCapacity("other") )

      val result = Json.toJson[Metadata](metadata)
      result.getClass mustBe classOf[JsObject]
      result mustBe Json.parse(expectedJson)
    }

    "Putting Director in Other should return Director" in {
      val expectedJson : String = s"""{
                                      |  "businessType" : "Limited company",
                                      |  "sessionId" : "session-123",
                                      |  "credentialId" : "cred-123",
                                      |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                                      |  "submissionFromAgent": false,
                                      |  "language" : "ENG",
                                      |  "completionCapacity" : "Director",
                                      |  "declareAccurateAndComplete": true
                                      |}""".stripMargin

      val metadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), CompletionCapacity("Director") )

      val result = Json.toJson[Metadata](metadata)
      result.getClass mustBe classOf[JsObject]
      result mustBe Json.parse(expectedJson)
    }

  }

  "The Interim Registration corporationTax model" must {
    "Produce valid JSON for a fuller model" in {
      val expectedJson : String = s"""{
                                      |  "companyOfficeNumber" : "623",
                                      |  "hasCompanyTakenOverBusiness" : false,
                                      |  "companyMemberOfGroup" : false,
                                      |  "companiesHouseCompanyName" : "DG Limited",
                                      |  "returnsOnCT61" : false,
                                      |  "companyACharity" : false,
                                      |  "businessAddress" : {
                                      |                       "line1" : "1 Acacia Avenue",
                                      |                       "line2" : "Hollinswood",
                                      |                       "line3" : "Telford",
                                      |                       "line4" : "Shropshire",
                                      |                       "postcode" : "TF3 4ER",
                                      |                       "country" : "England"
                                      |                           },
                                      |  "businessContactDetails" : {
                                      |                           "phoneNumber" : "0121 000 000",
                                      |                           "mobileNumber" : "0700 000 000",
                                      |                           "email" : "d@ddd.com"
                                      |                             }
                                      |}""".stripMargin
      val apiBusinessAddress = BusinessAddress(
        "1 Acacia Avenue",
        "Hollinswood",
        Some("Telford"),
        Some("Shropshire"),
        Some("TF3 4ER"),
        Some("England")
      )

      val apiBusinessContactContactDetails = BusinessContactDetails(
        Some("0121 000 000"),
        Some("0700 000 000"),
        Some("d@ddd.com")
      )

      val interimCorporationTax = InterimCorporationTax(
                                  "DG Limited",
                                  returnsOnCT61 = false,
                                  Some(apiBusinessAddress),
                                  apiBusinessContactContactDetails
                                )
      val result = Json.toJson[InterimCorporationTax](interimCorporationTax)
      result.getClass mustBe classOf[JsObject]
      result mustBe Json.parse(expectedJson)
    }

  }

  "The Interim API Registration model" must {

    "Be able to be parsed into JSON when groups is NONE" in {

      val expectedJson : String = s"""{  "acknowledgementReference" : "ackRef1",
                                      |  "registration" : {
                                      |  "metadata" : {
                                      |  "businessType" : "Limited company",
                                      |  "sessionId" : "session-123",
                                      |  "credentialId" : "cred-123",
                                      |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                                      |  "submissionFromAgent": false,
                                      |  "language" : "ENG",
                                      |  "completionCapacity" : "Director",
                                      |  "declareAccurateAndComplete": true
                                      |  },
                                      |  "corporationTax" : {
                                      |  "companyOfficeNumber" : "623",
                                      |  "hasCompanyTakenOverBusiness" : false,
                                      |  "companyMemberOfGroup" : false,
                                      |  "companiesHouseCompanyName" : "DG Limited",
                                      |  "returnsOnCT61" : false,
                                      |  "companyACharity" : false,
                                      |  "businessAddress" : {
                                      |                       "line1" : "1 Acacia Avenue",
                                      |                       "line2" : "Hollinswood",
                                      |                       "line3" : "Telford",
                                      |                       "line4" : "Shropshire",
                                      |                       "postcode" : "TF3 4ER",
                                      |                       "country" : "England"
                                      |                           },
                                      |  "businessContactDetails" : {
                                      |                           "phoneNumber" : "0121 000 000",
                                      |                           "mobileNumber" : "0700 000 000",
                                      |                           "email" : "d@ddd.com"
                                      |                             }
                                      |                             }
                                      |  }
                                      |}""".stripMargin

      val testMetadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), Director )
      val businessAddress = BusinessAddress(
        "1 Acacia Avenue",
        "Hollinswood",
        Some("Telford"),
        Some("Shropshire"),
        Some("TF3 4ER"),
        Some("England")
      )

      val businessContactDetails = BusinessContactDetails(
        Some("0121 000 000"),
        Some("0700 000 000"),
        Some("d@ddd.com")
      )

      val testInterimCorporationTax = InterimCorporationTax(
        "DG Limited",
        returnsOnCT61 = false,
        Some(businessAddress),
        businessContactDetails,
        groups = None
      )

      val testModel1 = InterimApiRegistration( "ackRef1", testMetadata, testInterimCorporationTax)

      val result = Json.toJson[InterimApiRegistration](testModel1)
      result.getClass mustBe classOf[JsObject]
      result mustBe Json.parse(expectedJson)
    }
    "Be able to be parsed into JSON when groups is Some and full" in {
      val expectedJson : String = s"""{  "acknowledgementReference" : "ackRef1",
                                     |  "registration" : {
                                     |  "metadata" : {
                                     |  "businessType" : "Limited company",
                                     |  "sessionId" : "session-123",
                                     |  "credentialId" : "cred-123",
                                     |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                                     |  "submissionFromAgent": false,
                                     |  "language" : "ENG",
                                     |  "completionCapacity" : "Director",
                                     |  "declareAccurateAndComplete": true
                                     |  },
                                     |  "corporationTax" : {
                                     |  "companyOfficeNumber" : "623",
                                     |  "hasCompanyTakenOverBusiness" : false,
                                     |  "companyMemberOfGroup" : true,
                                     |  "groupDetails" : {
                                     |    "parentCompanyName" : "testParentCompanyName",
                                     |    "groupAddress" : {
                                     |                      "line1" : "Line 1",
                                     |                       "line2" : "Line 2",
                                     |                       "line3" : "Telford",
                                     |                       "line4" : "Shropshire",
                                     |                       "postcode" : "ZZ1 1ZZ"
                                     |    },
                                     |    "parentUTR" : "1234567890"
                                     |  },
                                     |  "companiesHouseCompanyName" : "DG Limited",
                                     |  "returnsOnCT61" : false,
                                     |  "companyACharity" : false,
                                     |  "businessAddress" : {
                                     |                       "line1" : "1 Acacia Avenue",
                                     |                       "line2" : "Hollinswood",
                                     |                       "line3" : "Telford",
                                     |                       "line4" : "Shropshire",
                                     |                       "postcode" : "TF3 4ER",
                                     |                       "country" : "England"
                                     |                           },
                                     |  "businessContactDetails" : {
                                     |                           "phoneNumber" : "0121 000 000",
                                     |                           "mobileNumber" : "0700 000 000",
                                     |                           "email" : "d@ddd.com"
                                     |                             }
                                     |                             }
                                     |  }
                                     |}""".stripMargin

      val testMetadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), Director )
      val businessAddress = BusinessAddress(
        "1 Acacia Avenue",
        "Hollinswood",
        Some("Telford"),
        Some("Shropshire"),
        Some("TF3 4ER"),
        Some("England")
      )

      val businessContactDetails = BusinessContactDetails(
        Some("0121 000 000"),
        Some("0700 000 000"),
        Some("d@ddd.com")
      )
      val validGroups = Some(Groups(
        groupRelief = true,
        nameOfCompany = Some(GroupCompanyName("testParentCompanyName", GroupCompanyNameEnum.Other)),
        addressAndType = Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF,BusinessAddress(
          "Line 1",
          "Line 2",
          Some("Telford"),
          Some("Shropshire"),
          Some("ZZ1 1ZZ"),
          None
        ))),
        Some(GroupUTR(Some("1234567890")))
      ))

      val testInterimCorporationTax = InterimCorporationTax(
        "DG Limited",
        returnsOnCT61 = false,
        Some(businessAddress),
        businessContactDetails,
        groups = validGroups
      )
      val testModel1 = InterimApiRegistration( "ackRef1", testMetadata, testInterimCorporationTax)

      val result = Json.toJson[InterimApiRegistration](testModel1)
      result.getClass mustBe classOf[JsObject]
      result mustBe Json.parse(expectedJson)
    }

    "Be able to be parsed into JSON when groups is Some but the group relief has been selected as false by the user" in {
      val expectedJson : String = s"""{  "acknowledgementReference" : "ackRef1",
                                     |  "registration" : {
                                     |  "metadata" : {
                                     |  "businessType" : "Limited company",
                                     |  "sessionId" : "session-123",
                                     |  "credentialId" : "cred-123",
                                     |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                                     |  "submissionFromAgent": false,
                                     |  "language" : "ENG",
                                     |  "completionCapacity" : "Director",
                                     |  "declareAccurateAndComplete": true
                                     |  },
                                     |  "corporationTax" : {
                                     |  "companyOfficeNumber" : "623",
                                     |  "hasCompanyTakenOverBusiness" : false,
                                     |  "companyMemberOfGroup" : false,
                                     |  "companiesHouseCompanyName" : "DG Limited",
                                     |  "returnsOnCT61" : false,
                                     |  "companyACharity" : false,
                                     |  "businessAddress" : {
                                     |                       "line1" : "1 Acacia Avenue",
                                     |                       "line2" : "Hollinswood",
                                     |                       "line3" : "Telford",
                                     |                       "line4" : "Shropshire",
                                     |                       "postcode" : "TF3 4ER",
                                     |                       "country" : "England"
                                     |                           },
                                     |  "businessContactDetails" : {
                                     |                           "phoneNumber" : "0121 000 000",
                                     |                           "mobileNumber" : "0700 000 000",
                                     |                           "email" : "d@ddd.com"
                                     |                             }
                                     |                             }
                                     |  }
                                     |}""".stripMargin

      val testMetadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), Director )
      val businessAddress = BusinessAddress(
        "1 Acacia Avenue",
        "Hollinswood",
        Some("Telford"),
        Some("Shropshire"),
        Some("TF3 4ER"),
        Some("England")
      )

      val businessContactContactDetails = BusinessContactDetails(
        Some("0121 000 000"),
        Some("0700 000 000"),
        Some("d@ddd.com")
      )
      val validGroups = Some(Groups(
        groupRelief = false,None,None,None))

      val testInterimCorporationTax = InterimCorporationTax(
        "DG Limited",
        returnsOnCT61 = false,
        Some(businessAddress),
        businessContactContactDetails,
        groups = validGroups
      )
      val testModel1 = InterimApiRegistration( "ackRef1", testMetadata, testInterimCorporationTax)

      val result = Json.toJson[InterimApiRegistration](testModel1)
      result.getClass mustBe classOf[JsObject]
      result mustBe Json.parse(expectedJson)
    }
    "output valid json but group utr was empty" in {
      val expectedJson : String = s"""{  "acknowledgementReference" : "ackRef1",
                                     |  "registration" : {
                                     |  "metadata" : {
                                     |  "businessType" : "Limited company",
                                     |  "sessionId" : "session-123",
                                     |  "credentialId" : "cred-123",
                                     |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                                     |  "submissionFromAgent": false,
                                     |  "language" : "ENG",
                                     |  "completionCapacity" : "Director",
                                     |  "declareAccurateAndComplete": true
                                     |  },
                                     |  "corporationTax" : {
                                     |  "companyOfficeNumber" : "623",
                                     |  "hasCompanyTakenOverBusiness" : false,
                                     |  "companyMemberOfGroup" : true,
                                     |  "groupDetails" : {
                                     |    "parentCompanyName" : "testParentCompanyName",
                                     |    "groupAddress" : {
                                     |                      "line1" : "Line 1",
                                     |                      "line2" : "Line 2",
                                     |                      "line3" : "Telford",
                                     |                      "line4" : "Shropshire",
                                     |                      "postcode" : "ZZ1 1ZZ",
                                     |                      "country" : "UK"
                                     |    }
                                     |  },
                                     |  "companiesHouseCompanyName" : "DG Limited",
                                     |  "returnsOnCT61" : false,
                                     |  "companyACharity" : false,
                                     |  "businessAddress" : {
                                     |                       "line1" : "1 Acacia Avenue",
                                     |                       "line2" : "Hollinswood",
                                     |                       "line3" : "Telford",
                                     |                       "line4" : "Shropshire",
                                     |                       "postcode" : "TF3 4ER",
                                     |                       "country" : "England"
                                     |                           },
                                     |  "businessContactDetails" : {
                                     |                           "phoneNumber" : "0121 000 000",
                                     |                           "mobileNumber" : "0700 000 000",
                                     |                           "email" : "d@ddd.com"
                                     |                             }
                                     |                             }
                                     |  }
                                     |}""".stripMargin

      val testMetadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), Director )
      val businessAddress = BusinessAddress(
        "1 Acacia Avenue",
        "Hollinswood",
        Some("Telford"),
        Some("Shropshire"),
        Some("TF3 4ER"),
        Some("England")
      )

      val businessContactContactDetails = BusinessContactDetails(
        Some("0121 000 000"),
        Some("0700 000 000"),
        Some("d@ddd.com")
      )
      val validGroups = Some(Groups(
        groupRelief = true,
        nameOfCompany = Some(GroupCompanyName("testParentCompanyName", GroupCompanyNameEnum.Other)),
        addressAndType = Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF,BusinessAddress(
          "Line 1",
          "Line 2",
          Some("Telford"),
          Some("Shropshire"),
          Some("ZZ1 1ZZ"),
          Some("UK")
        ))),
        Some(GroupUTR(None))
      ))

      val testInterimCorporationTax = InterimCorporationTax(
        "DG Limited",
        returnsOnCT61 = false,
        Some(businessAddress),
        businessContactContactDetails,
        groups = validGroups
      )
      val testModel1 = InterimApiRegistration( "ackRef1", testMetadata, testInterimCorporationTax)

      val result = Json.toJson[InterimApiRegistration](testModel1)
      result.getClass mustBe classOf[JsObject]
      result mustBe Json.parse(expectedJson)
    }


    "Be able to be parsed into JSON when we have a takeover block" in {
      val expectedJson : String = s"""{ "acknowledgementReference" : "ackRef1",
                                     |  "registration" : {
                                     |  "metadata" : {
                                     |  "businessType" : "Limited company",
                                     |  "sessionId" : "session-123",
                                     |  "credentialId" : "cred-123",
                                     |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                                     |  "submissionFromAgent": false,
                                     |  "language" : "ENG",
                                     |  "completionCapacity" : "Director",
                                     |  "declareAccurateAndComplete": true
                                     |  },
                                     |  "corporationTax" : {
                                     |  "companyOfficeNumber" : "623",
                                     |  "hasCompanyTakenOverBusiness" : true,
                                     |  "businessTakeOverDetails" : {
                                     |  "businessNameLine1" : "Takeover name",
                                     |  "businessTakeoverAddress" : {
                                     |                      "line1" : "Takeover 1",
                                     |                      "line2" : "Takeover 2",
                                     |                      "line3" : "TTelford",
                                     |                      "line4" : "TShropshire",
                                     |                      "postcode" : "TO1 1ZZ",
                                     |                      "country" : "UK"
                                     |                      },
                                     |  "prevOwnersName" : "prev name",
                                     |  "prevOwnerAddress" : {
                                     |                      "line1" : "Prev 1",
                                     |                      "line2" : "Prev 2",
                                     |                      "line3" : "PTelford",
                                     |                      "line4" : "PShropshire",
                                     |                      "postcode" : "PR1 1ZZ",
                                     |                      "country" : "UK"
                                     |                      }
                                     |  },
                                     |  "companyMemberOfGroup" : true,
                                     |  "groupDetails" : {
                                     |    "parentCompanyName" : "testParentCompanyName",
                                     |    "groupAddress" : {
                                     |                       "line1" : "Line 1",
                                     |                       "line2" : "Line 2",
                                     |                       "line3" : "Telford",
                                     |                       "line4" : "Shropshire",
                                     |                       "postcode" : "ZZ1 1ZZ"
                                     |                       },
                                     |    "parentUTR" : "1234567890"
                                     |  },
                                     |  "companiesHouseCompanyName" : "DG Limited",
                                     |  "returnsOnCT61" : false,
                                     |  "companyACharity" : false,
                                     |  "businessAddress" : {
                                     |                       "line1" : "1 Acacia Avenue",
                                     |                       "line2" : "Hollinswood",
                                     |                       "line3" : "Telford",
                                     |                       "line4" : "Shropshire",
                                     |                       "postcode" : "TF3 4ER",
                                     |                       "country" : "England"
                                     |                           },
                                     |  "businessContactDetails" : {
                                     |                           "phoneNumber" : "0121 000 000",
                                     |                           "mobileNumber" : "0700 000 000",
                                     |                           "email" : "d@ddd.com"
                                     |                             }
                                     |                             }
                                     |  }
                                     |}""".stripMargin

      val testMetadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), Director )
      val businessAddress = BusinessAddress(
        "1 Acacia Avenue",
        "Hollinswood",
        Some("Telford"),
        Some("Shropshire"),
        Some("TF3 4ER"),
        Some("England")
      )

      val businessContactContactDetails = BusinessContactDetails(
        Some("0121 000 000"),
        Some("0700 000 000"),
        Some("d@ddd.com")
      )
      val validGroups = Some(Groups(
        groupRelief = true,
        nameOfCompany = Some(GroupCompanyName("testParentCompanyName", GroupCompanyNameEnum.Other)),
        addressAndType = Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF,BusinessAddress(
          "Line 1",
          "Line 2",
          Some("Telford"),
          Some("Shropshire"),
          Some("ZZ1 1ZZ"),
          None
        ))),
        Some(GroupUTR(Some("1234567890")))
      ))

      val validTakeover = Some(TakeoverDetails(
        replacingAnotherBusiness = true,
        businessName = Some("Takeover name@?><;:+/=(),.!¥#_•€&%£$[]{}~*«»"),
        businessTakeoverAddress = Some(Address(
          "Takeover 1",
          "Takeover 2",
          Some("TTelford"),
          Some("TShropshire"),
          Some("TO1 1ZZ"),
          Some("UK")
        )),
        prevOwnersName = Some("prev name"),
        prevOwnersAddress = Some(Address(
          "Prev 1",
          "Prev 2",
          Some("PTelford"),
          Some("PShropshire"),
          Some("PR1 1ZZ"),
          Some("UK")
        ))
      ))

      val testInterimCorporationTax = InterimCorporationTax(
        "DG Limited",
        returnsOnCT61 = false,
        Some(businessAddress),
        businessContactContactDetails,
        groups = validGroups,
        takeOver = validTakeover
      )
      val testModel1 = InterimApiRegistration( "ackRef1", testMetadata, testInterimCorporationTax)

      val result = Json.toJson[InterimApiRegistration](testModel1)
      result.getClass mustBe classOf[JsObject]
      result mustBe Json.parse(expectedJson)
    }








    "should not parse empty strings" in {
      val expectedJson : String = s"""{  "acknowledgementReference" : "ackRef1",
                                      |  "registration" : {
                                      |  "metadata" : {
                                      |  "businessType" : "Limited company",
                                      |  "sessionId" : "session-123",
                                      |  "credentialId" : "cred-123",
                                      |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                                      |  "submissionFromAgent": false,
                                      |  "language" : "ENG",
                                      |  "completionCapacity" : "Director",
                                      |  "declareAccurateAndComplete": true
                                      |  },
                                      |  "corporationTax" : {
                                      |  "companyOfficeNumber" : "623",
                                      |  "hasCompanyTakenOverBusiness" : false,
                                      |  "companyMemberOfGroup" : false,
                                      |  "companiesHouseCompanyName" : "DG Limited",
                                      |  "returnsOnCT61" : false,
                                      |  "companyACharity" : false,
                                      |  "businessContactDetails" : {
                                      |                             "email" : "d@ddd.com"
                                      |                             }
                                      |                           }
                                      |  }
                                      |}""".stripMargin

      val testMetadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), Director )

      val businessContactContactDetails = BusinessContactDetails(
        None,
        None,
        Some("d@ddd.com")
      )

      val testInterimCorporationTax = InterimCorporationTax(
        "DG Limited",
        returnsOnCT61 = false,
        None,
        businessContactContactDetails
      )

      val testModel1 = InterimApiRegistration( "ackRef1", testMetadata, testInterimCorporationTax)

      val result = Json.toJson[InterimApiRegistration](testModel1)
      result.getClass mustBe classOf[JsObject]
      result mustBe Json.parse(expectedJson)
    }
    "replace diacritics with equivalent alpha characters for company name" in {
            val expectedJson : String = s"""{  "acknowledgementReference" : "ackRef1",
                                     |  "registration" : {
                                     |  "metadata" : {
                                     |  "businessType" : "Limited company",
                                     |  "sessionId" : "session-123",
                                     |  "credentialId" : "cred-123",
                                     |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                                     |  "submissionFromAgent": false,
                                     |  "language" : "ENG",
                                     |  "completionCapacity" : "Director",
                                     |  "declareAccurateAndComplete": true
                                     |  },
                                     |  "corporationTax" : {
                                     |  "companyOfficeNumber" : "623",
                                     |  "hasCompanyTakenOverBusiness" : false,
                                     |  "companyMemberOfGroup" : false,
                                     |  "companiesHouseCompanyName" : "ss Oscar eg ant",
                                     |  "returnsOnCT61" : false,
                                     |  "companyACharity" : false,
                                     |  "businessContactDetails" : {
                                     |                             "email" : "d@ddd.com"
                                     |                             }
                                     |                           }
                                     |  }
                                     |}""".stripMargin

      val testMetadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), Director )
      val businessContactContactDetails = BusinessContactDetails(
                None,
                None,
                Some("d@ddd.com")
                )
      
              val testInterimCorporationTax = InterimCorporationTax(
                "ß Ǭscar ég ànt",
                returnsOnCT61 = false,
                None,
                businessContactContactDetails
                )
      
              val testModel1 = InterimApiRegistration( "ackRef1", testMetadata, testInterimCorporationTax)
      
              val result = Json.toJson[InterimApiRegistration](testModel1)
            result.getClass mustBe classOf[JsObject]
            result mustBe Json.parse(expectedJson)
          }
    
          "strip  punctuation characters for company name" in {
            val expectedJson: String =
                s"""{  "acknowledgementReference" : "ackRef1",
                                     |  "registration" : {
                                     |  "metadata" : {
                                     |  "businessType" : "Limited company",
                                     |  "sessionId" : "session-123",
                                     |  "credentialId" : "cred-123",
                                     |  "formCreationTimestamp": "1970-01-01T00:00:00.000Z",
                                     |  "submissionFromAgent": false,
                                     |  "language" : "ENG",
                                     |  "completionCapacity" : "Director",
                                     |  "declareAccurateAndComplete": true
                                     |  },
                                     |  "corporationTax" : {
                                     |  "companyOfficeNumber" : "623",
                                     |  "hasCompanyTakenOverBusiness" : false,
                                     |  "companyMemberOfGroup" : false,
                                     |  "companiesHouseCompanyName" : "Test Company",
                                     |  "returnsOnCT61" : false,
                                     |  "companyACharity" : false,
                                     |  "businessContactDetails" : {
                                     |                             "email" : "d@ddd.com"
                                     |                             }
                                     |                           }
                                     |  }
                                     |}""".stripMargin
      
            val testMetadata = Metadata( "session-123", "cred-123", "ENG", Instant.ofEpochSecond(0), Director )

            val businessContactContactDetails = BusinessContactDetails(
                None,
                None,
                Some("d@ddd.com")
                )
      
              val testInterimCorporationTax = InterimCorporationTax(
                "[Test Company]»",
                returnsOnCT61 = false,
                None,
                businessContactContactDetails
                )
      
              val testModel1 = InterimApiRegistration( "ackRef1", testMetadata, testInterimCorporationTax)
      
              val result = Json.toJson[InterimApiRegistration](testModel1)
            result.getClass mustBe classOf[JsObject]
            result mustBe Json.parse(expectedJson)
          }
  }
}
