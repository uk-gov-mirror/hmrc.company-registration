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

package audit

import models.api._
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsObject, Json}

import java.time.Instant

class UserRegistrationSubmissionEventSpec extends PlaySpec {

  "UserRegistrationSubmissionEventDetail" must {

    val regId = "123456789"
    val authProviderId = "apid001"
    val ackRef = "BRCT1234"
    val timestamp = "2001-12-31T12:00:00.000Z"
    val uprn = "123456789123"
    val transactionId = "trans01234"

    "construct full json as per definition" in {

      val expected = Json.parse(
        s"""
           |{
           |   "journeyId": "$regId",
           |   "acknowledgementReference": "$ackRef",
           |   "desSubmissionState": "partial",
           |   "registrationMetadata": {
           |      "businessType": "Limited company",
           |      "authProviderId": "$authProviderId",
           |      "formCreationTimestamp": "$timestamp",
           |      "submissionFromAgent": false,
           |      "language": "eng",
           |      "completionCapacity": "Director",
           |      "declareAccurateAndComplete": true
           |   },
           |   "corporationTax": {
           |      "companyOfficeNumber": "623",
           |      "hasCompanyTakenOverBusiness": false,
           |      "companyMemberOfGroup": false,
           |      "companiesHouseCompanyName": "Company Co",
           |      "returnsOnCT61": false,
           |      "companyACharity": false,
           |      "businessAddress": {
           |         "transactionId": "$transactionId",
           |         "addressEntryMethod": "LOOKUP",
           |         "addressLine1": "14 Matheson House",
           |         "addressLine2": "Grange Central",
           |         "addressLine3": "Town Centre",
           |         "addressLine4": "Telford",
           |         "postCode": "TF3 4ER",
           |         "country": "United Kingdom",
           |         "uprn": "$uprn"
           |      },
           |      "businessContactDetails": {
           |         "telephoneNumber": "0123456789",
           |         "mobileNumber": "0123456789",
           |         "emailAddress": "test@email.co.uk"
           |      }
           |   }
           |}
        """.stripMargin)

      val testModel = SubmissionEventDetail(
        regId,
        authProviderId,
        Some(transactionId),
        Some(uprn),
        "LOOKUP",
        Json.toJson(InterimApiRegistration(
          ackRef,
          Metadata(
            "sessionId", "credId", "eng", Instant.parse(timestamp), CompletionCapacity("Director")
          ),
          InterimCorporationTax(
            "Company Co",
            returnsOnCT61 = false,
            Some(BusinessAddress("14 Matheson House", "Grange Central", Some("Town Centre"), Some("Telford"), Some("TF3 4ER"), Some("United Kingdom"))),
            BusinessContactDetails(
              Some("0123456789"), Some("0123456789"), Some("test@email.co.uk")
            )
          )
        )).as[JsObject]
      )
      Json.toJson(testModel)(SubmissionEventDetail.writes) mustBe expected
    }

    "construct full json as per definition when the transaction ID is missing" in {

      val expected = Json.parse(
        s"""
           |{
           |   "journeyId": "$regId",
           |   "acknowledgementReference": "$ackRef",
           |   "desSubmissionState": "partial",
           |   "registrationMetadata": {
           |      "businessType": "Limited company",
           |      "authProviderId": "$authProviderId",
           |      "formCreationTimestamp": "$timestamp",
           |      "submissionFromAgent": false,
           |      "language": "eng",
           |      "completionCapacity": "Director",
           |      "declareAccurateAndComplete": true
           |   },
           |   "corporationTax": {
           |      "companyOfficeNumber": "623",
           |      "hasCompanyTakenOverBusiness": false,
           |      "companyMemberOfGroup": false,
           |      "companiesHouseCompanyName": "Company Co",
           |      "returnsOnCT61": false,
           |      "companyACharity": false,
           |      "businessContactDetails": {
           |         "telephoneNumber": "0123456789",
           |         "mobileNumber": "0123456789",
           |         "emailAddress": "test@email.co.uk"
           |      }
           |   }
           |}
        """.stripMargin)

      val testModel = SubmissionEventDetail(
        regId,
        authProviderId,
        None,
        Some(uprn),
        "LOOKUP",
        Json.toJson(InterimApiRegistration(
          ackRef,
          Metadata(
            "sessionId", "credId", "eng", Instant.parse(timestamp), CompletionCapacity("Director")
          ),
          InterimCorporationTax(
            "Company Co",
            returnsOnCT61 = false,
            Some(BusinessAddress("14 Matheson House", "Grange Central", Some("Town Centre"), Some("Telford"), Some("TF3 4ER"), Some("United Kingdom"))),
            BusinessContactDetails(
              Some("0123456789"), Some("0123456789"), Some("test@email.co.uk")
            )
          )
        )).as[JsObject]
      )
      Json.toJson(testModel)(SubmissionEventDetail.writes) mustBe expected
    }
  }
}
