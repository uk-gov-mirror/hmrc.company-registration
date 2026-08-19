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

package test.fixtures

import java.util.UUID
import auth.CryptoSCRS
import play.api.Configuration
import play.api.libs.json._
import uk.gov.hmrc.crypto.{ApplicationCrypto, Decrypter, Encrypter}

object CorporationTaxRegistrationFixture {

  val instanceOfCrypto: CryptoSCRS = new CryptoSCRS {
    val crypto: Encrypter with Decrypter = new ApplicationCrypto(Configuration("json.encryption.key" -> "MTIzNDU2Nzg5MDEyMzQ1Ng==").underlying).JsonCrypto
  }
  val cryptoFormat: Format[String] = Format(instanceOfCrypto.rds, instanceOfCrypto.wts)

  def ctRegistrationJson(regId: String = UUID.randomUUID().toString,
                         status: String = "draft",
                         createdTime: Long = 1504532988261L,
                         lastSignedIn: Long = 1515151515151L,
                         malform: Option[JsObject] = None): JsObject = Json.parse(
    s"""
       |{
       | "internalId":"testID",
       | "registrationID":"$regId",
       | "status":"$status",
       | "formCreationTimestamp":"testDateTime",
       | "language":"en",
       | "acknowledgementReferences":{
       |   "ct-utr":${cryptoFormat.writes("ctutr")},
       |   "timestamp":"timestamp",
       |   "status":"draft"
       | },
       | "confirmationReferences":{
       |   "acknowledgement-reference":"test-ack-ref",
       |   "transaction-id":"txId"
       | },
       | "companyDetails":{
       |   "companyName":"testCompanyName",
       |   "cHROAddress":{
       |     "premises":"Premises",
       |     "address_line_1":"Line 1",
       |     "address_line_2":"Line 2",
       |     "country":"Country",
       |     "locality":"Locality",
       |     "po_box":"PO box",
       |     "postal_code":"Post code",
       |     "region":"Region"
       |   },
       |   "pPOBAddress":{
       |     "addressType":"MANUAL",
       |     "address":{
       |       "addressLine1":"10 test street",
       |       "addressLine2":"test town",
       |       "addressLine3":"test area",
       |       "addressLine4":"test county",
       |       "postCode":"XX1 1ZZ",
       |       "country":"test country",
       |       "txid":"txid"
       |     }
       |   },
       | "jurisdiction":"testJurisdiction"
       | },
       | "tradingDetails":{
       |   "regularPayments":"true"
       | },
       | "contactDetails":{
       |   "contactDaytimeTelephoneNumber":"0123456789",
       |   "contactMobileNumber":"0123456789",
       |   "contactEmail":"test@email.co.uk"
       | },
       | "createdTime":$createdTime,
       | "lastSignedIn":$lastSignedIn
       |}
    """.stripMargin).as[JsObject].deepMerge(malform.getOrElse(Json.obj()))
}
