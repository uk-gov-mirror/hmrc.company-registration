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

package test.api

import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.stubbing.StubMapping
import com.github.tomakehurst.wiremock.verification.LoggedRequest
import config.LangConstants
import models.RegistrationStatus.{DRAFT, HELD, LOCKED}
import models._
import org.mongodb.scala.result.InsertOneResult
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.crypto.DefaultCookieSigner
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.ws.WSResponse
import play.api.test.Helpers._
import repositories.CorporationTaxRegistrationMongoRepository
import test.itutil.WiremockHelper.{stubGet, stubPost}
import test.itutil.{IntegrationSpecBase, LoginStub, MongoIntegrationSpec, WiremockHelper}
import uk.gov.hmrc.http.{HeaderNames => GovHeaderNames}

import java.time.Instant
import java.util
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ProcessIncorporationsControllerISpec extends IntegrationSpecBase with MongoIntegrationSpec with LoginStub {
  lazy val defaultCookieSigner: DefaultCookieSigner = app.injector.instanceOf[DefaultCookieSigner]
  val mockHost: String = WiremockHelper.wiremockHost
  val mockPort: Int = WiremockHelper.wiremockPort
  val mockUrl = s"http://$mockHost:$mockPort"

  val additionalConfiguration: Map[String, Any] = Map(
    "auditing.consumer.baseUri.host" -> s"$mockHost",
    "auditing.consumer.baseUri.port" -> s"$mockPort",
    "microservice.services.auth.host" -> s"$mockHost",
    "microservice.services.auth.port" -> s"$mockPort",
    "microservice.services.email.sendEmailURL" -> s"$mockUrl/hmrc/email",
    "microservice.services.address-line-4-fix.regId" -> s"999",
    "microservice.services.address-line-4-fix.address-line-4" -> s"dGVzdEFMNA==",
    "microservice.services.check-submission-job.schedule.blockage-logging-day" -> s"MON,TUE,WED,THU,FRI",
    "microservice.services.check-submission-job.schedule.blockage-logging-time" -> s"00:00:00_01:00:00",
    "microservice.services.des-service.host" -> s"$mockHost",
    "microservice.services.des-service.port" -> s"$mockPort",
    "microservice.services.des-service.url" -> s"$mockUrl/business-registration/corporation-tax",
    "microservice.services.des-service.environment" -> "local",
    "microservice.services.des-service.authorization-token" -> "testAuthToken",
    "microservice.services.des-topup-service.host" -> mockHost,
    "microservice.services.des-topup-service.port" -> mockPort,
    "microservice.services.doNotIndendToTradeDefaultDate" -> "MTkwMC0wMS0wMQ==",
    "microservice.services.incorporation-information.host" -> s"$mockHost",
    "microservice.services.incorporation-information.port" -> s"$mockPort",
    "microservice.services.business-registration.host" -> s"$mockHost",
    "microservice.services.business-registration.port" -> s"$mockPort"
  )

  override implicit lazy val app: Application = new GuiceApplicationBuilder()
    .configure(additionalConfiguration)
    .build()

  private def client(path: String) = ws.url(s"http://localhost:$port/company-registration/corporation-tax-registration$path").
    withFollowRedirects(false).
    withHttpHeaders(
      "Content-Type" -> "application/json",
      GovHeaderNames.authorisation -> "Bearer123"
    )

  class Setup {
    val ctRepository: CorporationTaxRegistrationMongoRepository = app.injector.instanceOf[CorporationTaxRegistrationMongoRepository]
    ctRepository.deleteAll
    await(ctRepository.ensureIndexes)

    def setupCTRegistration(reg: CorporationTaxRegistration): InsertOneResult = ctRepository.insert(reg)
  }

  val regId = "reg-id-12345"
  val internalId = "int-id-12345"
  val transId = "trans-id-2345"
  val payRef = "pay-ref-2345"
  val ackRef = "BRCT00000000001"
  val activeDate = "2017-07-23"

  val heldRegistration: CorporationTaxRegistration = CorporationTaxRegistration(
    internalId = internalId,
    registrationID = regId,
    status = HELD,
    formCreationTimestamp = "2001-12-31T12:00:00Z",
    language = LangConstants.english,
    confirmationReferences = Some(ConfirmationReferences(
      acknowledgementReference = ackRef,
      transactionId = transId,
      paymentReference = Some(payRef),
      paymentAmount = Some("12")
    )),
    accountingDetails = Some(AccountingDetails(
      status = AccountingDetails.FUTURE_DATE,
      activeDate = Some(activeDate))),
    verifiedEmail = Some(Email(
      address = "testEmail@address.com",
      emailType = "testType",
      linkSent = true,
      verified = true,
      returnLinkEmailSent = true
    )),
    companyDetails = Some(CompanyDetails(
      companyName = "testCompanyName",
      CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
      PPOB("MANUAL", Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"), None, "txid"))),
      jurisdiction = "testJurisdiction"
    )),
    tradingDetails = Some(TradingDetails(
      regularPayments = "true"
    )),
    contactDetails = Some(ContactDetails(
      phone = Some("02072899066"),
      mobile = Some("07567293726"),
      email = Some("test@email.co.uk")
    )),
    registrationProgress = Some("ho5"),
    acknowledgementReferences = None,
    accountsPreparation = None
  )

  val heldJson: JsObject = Json.parse(
    s"""
       |{
       |   "acknowledgementReference":"$ackRef",
       |   "registration":{
       |      "metadata":{
       |         "businessType":"Limited company",
       |         "submissionFromAgent":false,
       |         "declareAccurateAndComplete":true,
       |         "sessionId":"session-152ebc2f-8e0d-4137-8236-058b295fe52f",
       |         "credentialId":"cred-id-254524311264",
       |         "language":"ENG",
       |         "formCreationTimestamp":"2017-07-03T13:19:54.000+01:00",
       |         "completionCapacity":"Director"
       |      },
       |      "corporationTax":{
       |         "companyOfficeNumber":"623",
       |         "hasCompanyTakenOverBusiness":false,
       |         "companyMemberOfGroup":false,
       |         "companiesHouseCompanyName":"Company Name Ltd",
       |         "returnsOnCT61":false,
       |         "companyACharity":false,
       |         "businessAddress":{
       |            "line1":"12 address line 1",
       |            "line2":"address line 2",
       |            "line3":"address line 3",
       |            "line4":"address line 4",
       |            "postcode":"ZZ11 1ZZ"
       |         },
       |         "businessContactDetails":{
       |            "phoneNumber":"1234",
       |            "mobileNumber":"123456",
       |            "email":"6email@whatever.com"
       |         }
       |      }
       |   }
       |}
    """.stripMargin).as[JsObject]

  val subscriber = "SCRS"
  val regime = "CT"
  val crn = "012345"
  val prepDate = "2018-07-31"

  def jsonIncorpStatus(testIncorpDate: String): String =
    s"""
       |{
       |  "SCRSIncorpStatus": {
       |    "IncorpSubscriptionKey": {
       |      "subscriber":"$subscriber",
       |      "discriminator":"$regime",
       |      "transactionId": "$transId"
       |    },
       |    "SCRSIncorpSubscription":{
       |      "callbackUrl":"www.url.com"
       |    },
       |    "IncorpStatusEvent": {
       |      "status": "accepted",
       |      "crn": "$crn",
       |      "incorporationDate": ${Instant.parse(testIncorpDate + "T00:00:00.000Z").toEpochMilli}
       |    }
       |  }
       |}
      """.stripMargin

  def jsonIncorpStatusRejected: String =
    s"""
       |{
       |  "SCRSIncorpStatus": {
       |    "IncorpSubscriptionKey": {
       |      "subscriber":"$subscriber",
       |      "discriminator":"$regime",
       |      "transactionId": "$transId"
       |    },
       |    "SCRSIncorpSubscription":{
       |      "callbackUrl":"www.url.com"
       |    },
       |    "IncorpStatusEvent": {
       |      "status": "rejected"
       |    }
       |  }
       |}
      """.stripMargin

  def jsonAppendDataForSubmission(testIncorpDate: String): JsObject = Json.parse(
    s"""
       |{
       |   "registration": {
       |     "corporationTax": {
       |       "crn": "$crn",
       |       "companyActiveDate": "$testIncorpDate",
       |       "startDateOfFirstAccountingPeriod": "$testIncorpDate",
       |       "intendedAccountsPreparationDate": "$prepDate"
       |     }
       |   }
       |}
       """.stripMargin).as[JsObject]

  def stubApiPost(status: Int, submission: String): StubMapping = stubPost("/business-registration/corporation-tax", status, submission)

  def stubEmailPost(status: Int): StubMapping = stubPost("/hmrc/email", status, "")

  def stubApiTopUpPost(status: Int, submission: String): StubMapping = stubPost("/business-incorporation/corporation-tax", status, submission)

  "Process Incorporation" must {

    val processIncorpPath = "/process-incorp"
    val testIncorpDate = "2017-07-24"

    "send a top up submission to API with correct active date" when {
      "user has selected a Future Date for Accounting Dates before the incorporation date" in new Setup {

        ctRepository.insert(heldRegistration)

        setupSimpleAuthMocks()
        stubPost("/hmrc/email", 202, "")
        val ctSubmission: String = heldJson.deepMerge(jsonAppendDataForSubmission(testIncorpDate)).toString
        stubPost("/business-incorporation/corporation-tax", 200, ctSubmission)

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatus(testIncorpDate)))
        response.status mustBe 200

        val crPosts: util.List[LoggedRequest] = findAll(postRequestedFor(urlMatching(s"/business-incorporation/corporation-tax")))
        val captor: LoggedRequest = crPosts.get(0)
        val json: JsValue = Json.parse(captor.getBodyAsString)
        (json \ "corporationTax" \ "companyActiveDate").as[String] mustBe testIncorpDate
      }

      "user has selected a Future Date for Accounting Dates after the incorporation date" in new Setup {
        val testIncorpDate = "2017-07-22"

        ctRepository.insert(heldRegistration)

        setupSimpleAuthMocks()
        stubPost("/hmrc/email", 202, "")
        val ctSubmission: String = heldJson.deepMerge(jsonAppendDataForSubmission(activeDate)).toString
        stubPost("/business-incorporation/corporation-tax", 200, ctSubmission)

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatus(testIncorpDate)))
        response.status mustBe 200

        val crPosts: util.List[LoggedRequest] = findAll(postRequestedFor(urlMatching(s"/business-incorporation/corporation-tax")))
        val captor: LoggedRequest = crPosts.get(0)
        val json: JsValue = Json.parse(captor.getBodyAsString)
        (json \ "corporationTax" \ "companyActiveDate").as[String] mustBe activeDate
      }

      "user has selected When Registered for Accounting Dates" in new Setup {

        import AccountingDetails.WHEN_REGISTERED

        val testIncorpDate = "2017-07-25"
        val heldRegistration2: CorporationTaxRegistration = heldRegistration.copy(
          accountingDetails = Some(heldRegistration.accountingDetails.get.copy(status = WHEN_REGISTERED, activeDate = None))
        )

        ctRepository.insert(heldRegistration2)

        setupSimpleAuthMocks()
        stubPost("/hmrc/email", 202, "")
        val ctSubmission: String = heldJson.deepMerge(jsonAppendDataForSubmission(testIncorpDate)).toString
        stubPost("/business-incorporation/corporation-tax", 200, ctSubmission)

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatus(testIncorpDate)))
        response.status mustBe 200

        val crPosts: util.List[LoggedRequest] = findAll(postRequestedFor(urlMatching(s"/business-incorporation/corporation-tax")))
        val captor: LoggedRequest = crPosts.get(0)
        val json: JsValue = Json.parse(captor.getBodyAsString)
        (json \ "corporationTax" \ "companyActiveDate").as[String] mustBe testIncorpDate
      }

      "user has selected Not Intend to trade for Accounting Dates" in new Setup {

        import AccountingDetails.NOT_PLANNING_TO_YET

        val testIncorpDate = "2017-07-25"
        val activeDateToApi = "1900-01-01"
        val heldRegistration2: CorporationTaxRegistration = heldRegistration.copy(
          accountingDetails = Some(heldRegistration.accountingDetails.get.copy(status = NOT_PLANNING_TO_YET, activeDate = None))
        )

        ctRepository.insert(heldRegistration2)

        setupSimpleAuthMocks()
        stubPost("/hmrc/email", 202, "")
        val ctSubmission: String = heldJson.deepMerge(jsonAppendDataForSubmission(testIncorpDate)).toString
        stubPost("/business-incorporation/corporation-tax", 200, ctSubmission)

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatus(testIncorpDate)))
        response.status mustBe 200

        val crPosts: util.List[LoggedRequest] = findAll(postRequestedFor(urlMatching(s"/business-incorporation/corporation-tax")))
        val captor: LoggedRequest = crPosts.get(0)
        val json: JsValue = Json.parse(captor.getBodyAsString)
        (json \ "corporationTax" \ "companyActiveDate").as[String] mustBe activeDateToApi
      }
    }

    "handle an error response" when {

      "it is a 400" in new Setup {
        ctRepository.insert(heldRegistration)

        setupSimpleAuthMocks()
        stubPost("/hmrc/email", 202, "")
        stubPost("/business-incorporation/corporation-tax", 400, "")

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatus(testIncorpDate)))
        response.status mustBe 400
      }
      "it is a 409" in new Setup {
        ctRepository.insert(heldRegistration)

        setupSimpleAuthMocks()
        stubPost("/hmrc/email", 202, "")
        stubPost("/business-incorporation/corporation-tax", 409, "{}")

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatus(testIncorpDate)))
        response.status mustBe 200
      }
      "it is a 429" in new Setup {
        ctRepository.insert(heldRegistration)

        setupSimpleAuthMocks()
        stubPost("/hmrc/email", 202, "")
        stubPost(url = "/business-incorporation/corporation-tax", status = 429, responseBody = "{}")

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatus(testIncorpDate)))
        response.status mustBe 503
      }
      "it is a 499" in new Setup {
        ctRepository.insert(heldRegistration)

        setupSimpleAuthMocks()
        stubPost("/hmrc/email", 202, "")
        stubPost(url = "/business-incorporation/corporation-tax", status = 499, responseBody = "{}")

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatus(testIncorpDate)))
        response.status mustBe 502
      }
      "it is a 501" in new Setup {
        ctRepository.insert(heldRegistration)

        setupSimpleAuthMocks()
        stubPost("/hmrc/email", 202, "")
        stubPost(url = "/business-incorporation/corporation-tax", status = 501, responseBody = "{}")

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatus(testIncorpDate)))
        response.status mustBe 502
      }
      "registration is Locked by returning 202 to postpone the II notification" in new Setup {
        val businessRegistrationResponse: String =
          s"""
             |{
             |  "registrationID":"$regId",
             |  "formCreationTimestamp":"2017-08-09T11:48:20+01:00",
             |  "language":"en",
             |  "completionCapacity":"director"
             |}
        """.stripMargin

        ctRepository.insert(heldRegistration.copy(
          status = LOCKED, sessionIdentifiers = Some(SessionIds("sessID", "credID"))
        ))

        setupSimpleAuthMocks()

        stubPost("/hmrc/email", 202, "")

        stubGet(s"/business-registration/admin/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(s"/business-registration/corporation-tax", 200, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatus(testIncorpDate)))
        response.status mustBe 202

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.status mustBe HELD
      }
      "registration is Locked, but the update to held fails" in new Setup {
        val businessRegistrationResponse: String =
          s"""
             |{
             |  "registrationID":"$regId",
             |  "formCreationTimestamp":"2017-08-09T11:48:20+01:00",
             |  "language":"en",
             |  "completionCapacity":"director"
             |}
        """.stripMargin

        ctRepository.insert(heldRegistration.copy(
          status = LOCKED, sessionIdentifiers = Some(SessionIds("sessID", "credID"))
        ))

        setupSimpleAuthMocks()

        stubPost("/hmrc/email", 202, "")

        stubGet(s"/business-registration/admin/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(s"/business-registration/corporation-tax", 400, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatus(testIncorpDate)))
        response.status mustBe 400

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.status mustBe LOCKED
      }
      "registration is Locked, but cannot submit on behalf due to no session identifiers" in new Setup {
        val businessRegistrationResponse: String =
          s"""
             |{
             |  "registrationID":"$regId",
             |  "formCreationTimestamp":"2017-08-09T11:48:20+01:00",
             |  "language":"en",
             |  "completionCapacity":"director"
             |}
        """.stripMargin

        ctRepository.insert(heldRegistration.copy(
          status = LOCKED
        ))

        setupSimpleAuthMocks()

        stubPost("/hmrc/email", 202, "")

        stubGet(s"/business-registration/admin/business-tax-registration/$regId", 200, businessRegistrationResponse)

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatus(testIncorpDate)))
        response.status mustBe 200

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.status mustBe LOCKED
      }
      "registration is Locked by returning 202 to postpone the II rejected notification" in new Setup {
        val businessRegistrationResponse: String =
          s"""
             |{
             |  "registrationID":"$regId",
             |  "formCreationTimestamp":"2017-08-09T11:48:20+01:00",
             |  "language":"en",
             |  "completionCapacity":"director"
             |}
        """.stripMargin

        ctRepository.insert(heldRegistration.copy(
          status = LOCKED, sessionIdentifiers = Some(SessionIds("sessID", "credID"))
        ))

        setupSimpleAuthMocks()

        stubPost("/hmrc/email", 202, "")

        stubGet(s"/business-registration/admin/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(s"/business-registration/corporation-tax", 200, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatusRejected))
        response.status mustBe 202

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.status mustBe HELD
      }
      "registration is Locked, but the update to held fails on a rejection" in new Setup {
        val businessRegistrationResponse: String =
          s"""
             |{
             |  "registrationID":"$regId",
             |  "formCreationTimestamp":"2017-08-09T11:48:20+01:00",
             |  "language":"en",
             |  "completionCapacity":"director"
             |}
        """.stripMargin

        ctRepository.insert(heldRegistration.copy(
          status = LOCKED, sessionIdentifiers = Some(SessionIds("sessID", "credID"))
        ))

        setupSimpleAuthMocks()

        stubPost("/hmrc/email", 202, "")

        stubGet(s"/business-registration/admin/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(s"/business-registration/corporation-tax", 400, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(processIncorpPath).post(jsonIncorpStatusRejected))
        response.status mustBe 400

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.status mustBe LOCKED
      }
    }

    "clear down unnecessary CT Registration Information data when incorporation is rejected" when {
      "submission is HELD locally" in new Setup {
        setupSimpleAuthMocks()
        setupCTRegistration(heldRegistration)

        stubApiTopUpPost(200, """{"test":"json"}""")
        stubEmailPost(202)

        val response: Future[WSResponse] = client(processIncorpPath).post(jsonIncorpStatusRejected)

        await(response).status mustBe 200

        val reg :: _ = ctRepository.findAll

        reg.status mustBe "rejected"
        reg.confirmationReferences mustBe None
        reg.accountingDetails mustBe None
        reg.accountsPreparation mustBe None
        reg.verifiedEmail mustBe None
        reg.companyDetails mustBe None
        reg.tradingDetails mustBe None
        reg.contactDetails mustBe None
        reg.registrationID.isEmpty mustBe false
      }

      "submission has already been deleted" in new Setup {
        setupSimpleAuthMocks()

        val response: Future[WSResponse] = client(processIncorpPath).post(jsonIncorpStatusRejected)

        await(response).status mustBe 200
        ctRepository.count mustBe 0
      }

      "submission is LOCKED locally" in new Setup {
        val confRefsWithoutPayment: ConfirmationReferences = ConfirmationReferences(
          acknowledgementReference = ackRef,
          transactionId = transId,
          paymentReference = None,
          paymentAmount = None
        )

        setupSimpleAuthMocks()
        setupCTRegistration(heldRegistration.copy(status = "LOCKED", confirmationReferences = Some(confRefsWithoutPayment)))

        stubApiTopUpPost(200, """{"test":"json"}""")
        stubEmailPost(202)

        val response: Future[WSResponse] = client(processIncorpPath).post(jsonIncorpStatusRejected)

        await(response).status mustBe 200

        val reg :: _ = ctRepository.findAll

        reg.status mustBe "rejected"
        reg.confirmationReferences mustBe None
        reg.accountingDetails mustBe None
        reg.accountsPreparation mustBe None
        reg.verifiedEmail mustBe None
        reg.companyDetails mustBe None
        reg.tradingDetails mustBe None
        reg.contactDetails mustBe None
        reg.registrationID.isEmpty mustBe false
      }
    }

    "send a top-up submission to API if a matching held registration exists and a held submission does not exist" in new Setup {

      val jsonBodyFromII: String = jsonIncorpStatus(testIncorpDate)

      setupSimpleAuthMocks()
      setupCTRegistration(heldRegistration)

      stubApiTopUpPost(200, """{"test":"json"}""")
      stubEmailPost(202)

      val response: Future[WSResponse] = client(processIncorpPath).post(jsonBodyFromII)

      await(response).status mustBe 200

      val reg :: _ = ctRepository.findAll

      reg.status mustBe "submitted"
    }

    "NOT send a top-up submission to API if a matching registration exists as not held status" in new Setup {

      val jsonBodyFromII: String = jsonIncorpStatus(testIncorpDate)

      setupSimpleAuthMocks()
      setupCTRegistration(heldRegistration.copy(status = DRAFT))

      stubEmailPost(202)
      stubPost("/write/audit", 200, """{"x":2}""")

      val response: Future[WSResponse] = client(processIncorpPath).post(jsonBodyFromII)
      await(response).status mustBe 500
    }

    "return a 502 if the top-up submission to API fails, then return a 200 when retried and the submission to API is successful" in new Setup {

      val jsonBodyFromII: String = jsonIncorpStatus(testIncorpDate)

      setupSimpleAuthMocks()
      setupCTRegistration(heldRegistration)

      stubApiTopUpPost(502, """{"test":"json for 502"}""")
      stubEmailPost(202)

      val response1: Future[WSResponse] = client(processIncorpPath).post(jsonBodyFromII)

      await(response1).status mustBe 502

      val reg1 :: _ = ctRepository.findAll
      reg1.status mustBe "held"

      stubApiTopUpPost(200, """{"test":"json for 200"}""")

      val response2: Future[WSResponse] = client(processIncorpPath).post(jsonBodyFromII)

      await(response2).status mustBe 200

      val reg2 :: _ = ctRepository.findAll
      reg2.status mustBe "submitted"
    }

    "return a 200 when receiving a top up for an Accepted document" in new Setup {

      val jsonBodyFromII: String = jsonIncorpStatus(testIncorpDate)

      setupSimpleAuthMocks()
      setupCTRegistration(heldRegistration.copy(status = RegistrationStatus.ACKNOWLEDGED))

      val response1: Future[WSResponse] = client(processIncorpPath).post(jsonBodyFromII)

      await(response1).status mustBe 200

      val reg1 :: _ = ctRepository.findAll
      reg1.status mustBe "acknowledged"
    }
    "return a 200 when receiving a top up for an Accepted document when there is no CT document for this case" in new Setup {

      setupSimpleAuthMocks()
      val jsonBodyFromII: String = jsonIncorpStatus(testIncorpDate)

      setupSimpleAuthMocks()

      val response1: Future[WSResponse] = client(processIncorpPath).post(jsonBodyFromII)

      await(response1).status mustBe 200
    }
  }
}