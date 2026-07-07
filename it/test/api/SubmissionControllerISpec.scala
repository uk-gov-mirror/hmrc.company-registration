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

package api

import auth.CryptoSCRS
import com.github.tomakehurst.wiremock.client.WireMock._
import config.{LangConstants, MicroserviceAppConfig}
import models.RegistrationStatus._
import models._
import models.api.BusinessAddress
import org.mongodb.scala.result.InsertOneResult
import play.api.Application
import play.api.http.HeaderNames
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.crypto.DefaultCookieSigner
import play.api.libs.json._
import play.api.libs.ws.{WSRequest, WSResponse}
import play.api.test.Helpers._
import repositories.{CorporationTaxRegistrationMongoRepository, SequenceMongoRepository}
import test.itutil.WiremockHelper.{stubAuthorise, stubGet, stubPost}
import test.itutil.{IntegrationSpecBase, LoginStub, MongoIntegrationSpec, RequestFinder, WiremockHelper}
import uk.gov.hmrc.http.{HeaderNames => GovHeaderNames}

import scala.concurrent.ExecutionContext.Implicits.global

class SubmissionControllerISpec extends IntegrationSpecBase with LoginStub with RequestFinder with MongoIntegrationSpec {
  val mockHost: String = WiremockHelper.wiremockHost
  val mockPort: Int = WiremockHelper.wiremockPort
  val mockUrl: String = s"http://$mockHost:$mockPort"

  lazy val defaultCookieSigner: DefaultCookieSigner = app.injector.instanceOf[DefaultCookieSigner]
  lazy val appConfig = app.injector.instanceOf[MicroserviceAppConfig]


  val additionalConfiguration: Map[String, Any] = Map(
    "auditing.consumer.baseUri.host" -> s"$mockHost",
    "auditing.consumer.baseUri.port" -> s"$mockPort",
    "microservice.services.auth.host" -> s"$mockHost",
    "microservice.services.auth.port" -> s"$mockPort",
    "microservice.services.business-registration.host" -> s"$mockHost",
    "microservice.services.business-registration.port" -> s"$mockPort",
    "microservice.services.email.sendEmailURL" -> s"$mockUrl/hmrc/email",
    "microservice.services.address-line-4-fix.regId" -> s"999",
    "microservice.services.address-line-4-fix.address-line-4" -> s"dGVzdEFMNA==",
    "microservice.services.check-submission-job.schedule.blockage-logging-day" -> s"MON,TUE,WED,THU,FRI",
    "microservice.services.check-submission-job.schedule.blockage-logging-time" -> s"00:00:00_01:00:00",
    "microservice.services.des-service.host" -> s"$mockHost",
    "microservice.services.des-service.port" -> s"$mockPort",
    "microservice.services.incorporation-information.host" -> s"$mockHost",
    "microservice.services.incorporation-information.port" -> s"$mockPort",
    "microservice.services.des-service.url" -> s"$mockUrl/business-registration/corporation-tax",
    "microservice.services.des-service.environment" -> "local",
    "microservice.services.des-service.authorization-token" -> "testAuthToken",
    "microservice.services.des-topup-service.host" -> mockHost,
    "microservice.services.des-topup-service.port" -> mockPort,
    "microservice.services.hip.host" -> s"$mockHost",
    "microservice.services.hip.port" -> s"$mockPort"
  )

  override implicit lazy val app: Application = new GuiceApplicationBuilder()
    .configure(additionalConfiguration)
    .build()

  private def client(path: String): WSRequest = ws.url(s"http://localhost:$port/company-registration/corporation-tax-registration$path")
    .withFollowRedirects(false)
    .withHttpHeaders(
      "Content-Type" -> "application/json",
      HeaderNames.SET_COOKIE -> getSessionCookie(),
      GovHeaderNames.xSessionId -> SessionId,
      GovHeaderNames.authorisation -> "Bearer123"
    )

  class Setup {
    val crypto = app.injector.instanceOf[CryptoSCRS]
    val ctRepository: CorporationTaxRegistrationMongoRepository = app.injector.instanceOf[CorporationTaxRegistrationMongoRepository]
    val seqRepo: SequenceMongoRepository = app.injector.instanceOf[SequenceMongoRepository]

    ctRepository.deleteAll
    await(ctRepository.ensureIndexes)

    seqRepo.deleteAll
    await(seqRepo.ensureIndexes)

    def setupCTRegistration(reg: CorporationTaxRegistration): InsertOneResult = ctRepository.insert(reg)

    stubAuthorise(internalId)
  }

  val regId: String = "reg-id-12345"
  val internalId: String = "int-id-12345"
  val transId: String = "trans-id-2345"
  val ackRef: String = "BRCT00000000001"
  val activeDate: String = "2017-07-23"
  val payRef: String = "pay-ref-2345"
  val payAmount: String = "12"
  val ctutr: String = "CTUTR123456789"

  val confRefsWithPayment: ConfirmationReferences = ConfirmationReferences(
    acknowledgementReference = ackRef,
    transactionId = transId,
    paymentReference = Some(payRef),
    paymentAmount = Some(payAmount)
  )

  val draftRegistration: CorporationTaxRegistration = CorporationTaxRegistration(
    internalId = internalId,
    registrationID = regId,
    status = DRAFT,
    formCreationTimestamp = "2001-12-31T12:00:00Z",
    language = LangConstants.english,
    confirmationReferences = None,
    companyDetails = Some(CompanyDetails(
      companyName = "testCompanyName",
      CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
      PPOB("MANUAL", Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"), None, "txid"))),
      jurisdiction = "testJurisdiction"
    )),
    accountingDetails = Some(AccountingDetails(
      status = AccountingDetails.FUTURE_DATE,
      activeDate = Some("2019-12-31"))),
    tradingDetails = Some(TradingDetails(
      regularPayments = "true"
    )),
    contactDetails = Some(ContactDetails(
      phone = Some("02072899066"),
      mobile = Some("07567293726"),
      email = Some("test@email.co.uk")
    )),
    verifiedEmail = Some(Email(
      address = "testEmail@address.com",
      emailType = "testType",
      linkSent = true,
      verified = true,
      returnLinkEmailSent = true
    )),
    registrationProgress = Some("ho5"),
    acknowledgementReferences = None,
    accountsPreparation = None
  )

  val heldRegistration: CorporationTaxRegistration = CorporationTaxRegistration(
    internalId = internalId,
    registrationID = regId,
    status = HELD,
    formCreationTimestamp = "2001-12-31T12:00:00Z",
    language = LangConstants.english,
    confirmationReferences = Some(ConfirmationReferences(
      acknowledgementReference = ackRef,
      transactionId = transId,
      paymentReference = None,
      paymentAmount = None
    )),
    companyDetails = None,
    accountingDetails = Some(AccountingDetails(
      status = AccountingDetails.FUTURE_DATE,
      activeDate = Some(activeDate))),
    tradingDetails = None,
    contactDetails = None,
    verifiedEmail = Some(Email(
      address = "testEmail@address.com",
      emailType = "testType",
      linkSent = true,
      verified = true,
      returnLinkEmailSent = true
    )),
    registrationProgress = Some("ho5"),
    acknowledgementReferences = None,
    accountsPreparation = None
  )

  val validConfirmationReferences: ConfirmationReferences = ConfirmationReferences(
    acknowledgementReference = "BRCT12345678910",
    transactionId = "TX1",
    paymentReference = Some("PY1"),
    paymentAmount = Some("12.00")
  )

  val validAckRefs: AcknowledgementReferences = AcknowledgementReferences(
    ctUtr = Some(ctutr),
    timestamp = "856412556487",
    status = "success"
  )

  val fullCorporationTaxRegistration: CorporationTaxRegistration = CorporationTaxRegistration(
    internalId = internalId,
    registrationID = regId,
    status = ACKNOWLEDGED,
    formCreationTimestamp = "2001-12-31T12:00:00Z",
    language = LangConstants.english,
    acknowledgementReferences = Some(validAckRefs),
    confirmationReferences = Some(validConfirmationReferences),
    companyDetails = None,
    accountingDetails = None,
    tradingDetails = None,
    contactDetails = None
  )

  def jsonConfirmationRefs(ackReference: String = "", paymentRef: Option[String] = None, paymentAmount: Option[String] = None): String = {
    val confRefs = Json.obj(
      "acknowledgement-reference" -> "$ackReference",
      "transaction-id" -> s"$transId"
    )

    val json = confRefs ++
      paymentRef.fold(Json.obj())(ref => Json.obj("payment-reference" -> ref)) ++
      paymentAmount.fold(Json.obj())(tot => Json.obj("payment-amount" -> tot))

    json.toString
  }

  val ctSubmissionUrl: String =
    if (appConfig.useHip) "/RESTAdapter/business-registration/CT" else "/business-registration/corporation-tax"

  "handleUserSubmission" must {

    val authProviderId: String = "testAuthProviderId"
    val authorisedRetrievals: JsObject = Json.obj(
      "internalId" -> internalId,
      "optionalCredentials" -> Json.obj("providerId" -> authProviderId, "providerType" -> "testType"))

    "return Confirmation References when registration is in Held status" in new Setup {
      stubAuthorise(200, authorisedRetrievals)

      ctRepository.insert(heldRegistration.copy(confirmationReferences = Some(confRefsWithPayment)))

      val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs(ackRef, Some(payRef), Some(payAmount))))
      response.status mustBe 200
      response.json mustBe Json.toJson(confRefsWithPayment)
    }

    "update Confirmation References when registration is in Held status (new HO6)" in new Setup {
      stubAuthorise(200, authorisedRetrievals)

      ctRepository.insert(heldRegistration)

      val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs(ackRef, Some(payRef), Some(payAmount))))
      response.status mustBe 200
      response.json mustBe Json.toJson(confRefsWithPayment)

      ctRepository.findAll.head.confirmationReferences mustBe Some(confRefsWithPayment)
    }

    "submit to API" when {
      val businessRegistrationResponse = Json.obj(
        "registrationID" -> s"$regId",
        "formCreationTimestamp" -> "2017-08-09T11:48:20+01:00",
        "language" -> "en",
        "completionCapacity" -> "director"
      ).toString()

      "registration is in Draft status and update Confirmation References with Ack Ref and Payment infos (old HO6)" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        ctRepository.insert(draftRegistration)

        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 200, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs("", Some(payRef), Some(payAmount))))
        response.status mustBe 200
        response.json mustBe Json.toJson(confRefsWithPayment)

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithPayment)
        reg.sessionIdentifiers mustBe None
        reg.status mustBe HELD
      }

      "registration is in Draft status and update Confirmation References but API submission failed 403 (old HO6)" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        ctRepository.insert(draftRegistration)

        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 403, "")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs("", Some(payRef), Some(payAmount))))
        response.status mustBe 400

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithPayment)
        reg.sessionIdentifiers mustBe Some(SessionIds(SessionId, authProviderId))
        reg.status mustBe RegistrationStatus.LOCKED
      }

      "registration is in Draft status and update Confirmation References but API submission failed 429 (old HO6)" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        ctRepository.insert(draftRegistration)

        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 429, "")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs("", Some(payRef), Some(payAmount))))
        response.status mustBe 503

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithPayment)
        reg.sessionIdentifiers mustBe Some(SessionIds(SessionId, authProviderId))
        reg.status mustBe RegistrationStatus.LOCKED
      }

      "registration is in Locked status (old HO6)" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        val confRefsWithPayment: ConfirmationReferences = ConfirmationReferences(
          acknowledgementReference = ackRef,
          transactionId = transId,
          paymentReference = Some(payRef),
          paymentAmount = Some(payAmount)
        )
        val lockedRegistration: CorporationTaxRegistration = draftRegistration.copy(status = RegistrationStatus.LOCKED, confirmationReferences = Some(confRefsWithPayment))

        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        ctRepository.insert(lockedRegistration)

        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 200, """{"a": "b"}""")

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs(ackRef, Some(payRef), Some(payAmount))))
        response.status mustBe 200
        response.json mustBe Json.toJson(confRefsWithPayment)

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithPayment)
        reg.sessionIdentifiers mustBe None
        reg.status mustBe HELD
      }

      "registration is in Draft status and update Confirmation References with Ack Ref (new HO5-1)" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        val confRefsWithoutPayment: ConfirmationReferences = ConfirmationReferences(
          acknowledgementReference = ackRef,
          transactionId = transId,
          paymentReference = None,
          paymentAmount = None
        )

        ctRepository.insert(draftRegistration)

        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 200, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs()))
        response.status mustBe 200
        response.json mustBe Json.toJson(confRefsWithoutPayment)

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithoutPayment)
        reg.sessionIdentifiers mustBe None
        reg.status mustBe HELD
      }

      "registration is in Draft status, at 5-1, sending the RO address as the PPOB" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        val confRefsWithoutPayment: ConfirmationReferences = ConfirmationReferences(
          acknowledgementReference = ackRef,
          transactionId = transId,
          paymentReference = None,
          paymentAmount = None
        )

        ctRepository.insert(draftRegistration.copy(companyDetails =
          Some(CompanyDetails(
            companyName = "testCompanyName",
            CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("ZZ1 1ZZ"), Some("Region")),
            PPOB("RO", None),
            jurisdiction = "testJurisdiction"
          ))
        ))

        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 200, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs()))
        response.status mustBe 200
        response.json mustBe Json.toJson(confRefsWithoutPayment)

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithoutPayment)
        reg.sessionIdentifiers mustBe None
        reg.status mustBe HELD
      }

      "registration is in Draft status, at 5-1, sending the RO address as the PPOB, RO and takeover addresses have unormalised characters" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        val confRefsWithoutPayment: ConfirmationReferences = ConfirmationReferences(
          acknowledgementReference = ackRef,
          transactionId = transId,
          paymentReference = None,
          paymentAmount = None
        )

        val invalidAddress = Address(
          "<123> {ABC} !*^%$£",
          "BDT & CFD /|@",
          Some("A¥€ 1 «»;:"),
          Some("B~ ¬` 2^ -+=_:;"),
          Some("XX1 1ØZ"),
          Some("test coûntry")
        )

        val takeoverWithInvalidAddress: TakeoverDetails = TakeoverDetails(
          replacingAnotherBusiness = true,
          businessName = Some("Takeover name"),
          businessTakeoverAddress = Some(invalidAddress),
          prevOwnersName = Some("prev name"),
          prevOwnersAddress = Some(invalidAddress)
        )

        ctRepository.insert(draftRegistration.copy(companyDetails =
          Some(CompanyDetails(
            companyName = "testCompanyName",
            CHROAddress("<123> {ABC} !*^%$£", "BDT & CFD /|@", Some("A¥€ 1 «»;:"), "D£q|l", ":~#Rts!2", Some("B~ ¬` 2^ -+=_:;"), Some("XX1 1ØZ"), Some("test coûntry")),
            PPOB("RO", None),
            jurisdiction = "testJurisdiction"
          )),
          takeoverDetails = Some(takeoverWithInvalidAddress)
        ))

        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 200, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs()))
        response.status mustBe 200
        response.json mustBe Json.toJson(confRefsWithoutPayment)

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithoutPayment)
        reg.sessionIdentifiers mustBe None
        reg.status mustBe HELD

        val requestBody: JsObject = getPOSTRequestJsonBody(ctSubmissionUrl).as[JsObject]
        val registration: JsObject = (requestBody \ "registration" \ "metadata").as[JsObject] - "sessionId" - "formCreationTimestamp"
        val corp: JsObject = (requestBody \ "registration" \ "corporationTax").as[JsObject]
        val res: JsObject = requestBody - "registration" ++ Json.obj("registration" -> Json.obj("metadata" -> registration, "corporationTax" -> corp))

        val expectedJsonBody: JsObject =
          Json.obj(
            "acknowledgementReference" -> "BRCT00000000001",
            "registration" -> Json.obj(
              "metadata" -> Json.obj(
                "businessType" -> "Limited company",
                "submissionFromAgent" -> false,
                "declareAccurateAndComplete" -> true,
                "credentialId" -> "testAuthProviderId",
                "language" -> "en",
                "completionCapacity" -> "Director"
              ),
              "corporationTax" -> Json.obj(
                "companyOfficeNumber" -> "623",
                "hasCompanyTakenOverBusiness" -> true,
                "companyMemberOfGroup" -> false,
                "companiesHouseCompanyName" -> "testCompanyName",
                "returnsOnCT61" -> true,
                "companyACharity" -> false,
                "businessAddress" -> Json.obj(
                  "line1" -> "123 ABC  BDT & CFD /",
                  "line2" -> "A 1 ,.",
                  "line3" -> ".Rts2",
                  "line4" -> "test country",
                  "postcode" -> "XX1 1OZ",
                  "country" -> "Dql"
                ),
                "businessContactDetails" -> Json.obj(
                  "phoneNumber" -> "02072899066",
                  "mobileNumber" -> "07567293726",
                  "email" -> "test@email.co.uk"
                ),
                "businessTakeOverDetails" -> Json.obj(
                  "businessNameLine1" -> "Takeover name",
                  "businessTakeoverAddress" -> Json.obj(
                    "country" -> "test country",
                    "line1" -> "123 ABC ",
                    "line2" -> "BDT & CFD /",
                    "line3" -> "A 1 ,.",
                    "line4" -> "B  2 -.,",
                    "postcode" -> "XX1 1OZ"
                  ),
                  "prevOwnerAddress" -> Json.obj(
                    "country" -> "test country",
                    "line1" -> "123 ABC ",
                    "line2" -> "BDT & CFD /",
                    "line3" -> "A 1 ,.",
                    "line4" -> "B  2 -.,",
                    "postcode" -> "XX1 1OZ"
                  ),
                  "prevOwnersName" -> "prev name"
                )
              )
            )
          )

        res mustBe expectedJsonBody

      }

      "registration is in Draft status, at 5-1, sending the PPOB address as the PPOB and PPOB has unormalised characters" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        val confRefsWithoutPayment: ConfirmationReferences = ConfirmationReferences(
          acknowledgementReference = ackRef,
          transactionId = transId,
          paymentReference = None,
          paymentAmount = None
        )

        ctRepository.insert(draftRegistration.copy(companyDetails =
          Some(CompanyDetails(
            companyName = "testCompanyName",
            CHROAddress("<123> {ABC} !*^%$£", "BDT & CFD /|@", Some("A¥€ 1 «»;:"), "D£q|l", ":~#Rts!2", Some("B~ ¬` 2^ -+=_:;"), Some("XX1 1ØZ"), Some("test coûntry")),
            PPOB("PPOB", Some(PPOBAddress("line1:", "line2;", Some("line3:;"), Some("line4\\"), Some("ZZ1 1ZZ"), Some("Country"), None, "txid"))),
            jurisdiction = "testJurisdiction"
          ))
        ))

        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 200, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs()))
        response.status mustBe 200
        response.json mustBe Json.toJson(confRefsWithoutPayment)

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithoutPayment)
        reg.sessionIdentifiers mustBe None
        reg.status mustBe HELD

        val requestBody: JsObject = getPOSTRequestJsonBody(ctSubmissionUrl).as[JsObject]
        val registration: JsObject = (requestBody \ "registration" \ "metadata").as[JsObject] - "sessionId" - "formCreationTimestamp"
        val corp: JsObject = (requestBody \ "registration" \ "corporationTax").as[JsObject]
        val res: JsObject = requestBody - "registration" ++ Json.obj("registration" -> Json.obj("metadata" -> registration, "corporationTax" -> corp))

        val expectedJsonBody: JsObject =
          Json.obj(
            "acknowledgementReference" -> "BRCT00000000001",
            "registration" -> Json.obj(
              "metadata" -> Json.obj(
                "businessType" -> "Limited company",
                "submissionFromAgent" -> false,
                "declareAccurateAndComplete" -> true,
                "credentialId" -> "testAuthProviderId",
                "language" -> "en",
                "completionCapacity" -> "Director"
              ),
              "corporationTax" -> Json.obj(
                "companyOfficeNumber" -> "623",
                "hasCompanyTakenOverBusiness" -> false,
                "companyMemberOfGroup" -> false,
                "companiesHouseCompanyName" -> "testCompanyName",
                "returnsOnCT61" -> true,
                "companyACharity" -> false,
                "businessAddress" -> Json.obj(
                  "line1" -> "line1.",
                  "line2" -> "line2,",
                  "line3" -> "line3.,",
                  "line4" -> "line4/",
                  "postcode" -> "ZZ1 1ZZ",
                  "country" -> "Country"
                ),
                "businessContactDetails" -> Json.obj(
                  "phoneNumber" -> "02072899066",
                  "mobileNumber" -> "07567293726",
                  "email" -> "test@email.co.uk"
                )
              )
            )
          )

        res mustBe expectedJsonBody
      }

      "registration is in Draft status, at 5-1, sending groups block" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        val confRefsWithoutPayment: ConfirmationReferences = ConfirmationReferences(
          acknowledgementReference = ackRef,
          transactionId = transId,
          paymentReference = None,
          paymentAmount = None
        )
        val validGroups: Option[Groups] = Some(Groups(
          groupRelief = true,
          nameOfCompany = Some(GroupCompanyName("testCompanyName", GroupCompanyNameEnum.Other)),
          addressAndType = Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF, BusinessAddress(
            "Line 1",
            "Line 2",
            Some("Telford"),
            Some("Shropshire"),
            Some("ZZ1 1ZZ"),
            None
          ))),
          Some(GroupUTR(Some("1234567890")))
        ))

        ctRepository.insert(
          draftRegistration.copy(
            companyDetails =
              Some(CompanyDetails(
                companyName = "testCompanyName",
                CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("ZZ1 1ZZ"), Some("Region")),
                PPOB("RO", None),
                jurisdiction = "testJurisdiction"
              )), groups = validGroups
          ))

        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 200, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs()))
        response.status mustBe 200
        response.json mustBe Json.toJson(confRefsWithoutPayment)

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithoutPayment)
        reg.sessionIdentifiers mustBe None
        reg.status mustBe HELD

        val requestBody: JsObject = getPOSTRequestJsonBody(ctSubmissionUrl).as[JsObject]
        val registration: JsObject = (requestBody \ "registration" \ "metadata").as[JsObject] - "sessionId" - "formCreationTimestamp"
        val corp: JsObject = (requestBody \ "registration" \ "corporationTax").as[JsObject]
        val res: JsObject = requestBody - "registration" ++ Json.obj("registration" -> Json.obj("metadata" -> registration, "corporationTax" -> corp))

        val expectedJsonBody: JsObject =
          Json.obj(
            "acknowledgementReference" -> "BRCT00000000001",
            "registration" -> Json.obj(
              "metadata" -> Json.obj(
                "businessType" -> "Limited company",
                "submissionFromAgent" -> false,
                "declareAccurateAndComplete" -> true,
                "credentialId" -> "testAuthProviderId",
                "language" -> "en",
                "completionCapacity" -> "Director"
              ),
              "corporationTax" -> Json.obj(
                "companyOfficeNumber" -> "623",
                "hasCompanyTakenOverBusiness" -> false,
                "companiesHouseCompanyName" -> "testCompanyName",
                "returnsOnCT61" -> true,
                "companyACharity" -> false,
                "companyMemberOfGroup" -> true,
                "groupDetails" -> Json.obj(
                  "parentCompanyName" -> "testCompanyName",
                  "groupAddress" -> Json.obj(
                    "line1" -> "Line 1",
                    "line2" -> "Line 2",
                    "line3" -> "Telford",
                    "line4" -> "Shropshire",
                    "postcode" -> "ZZ1 1ZZ"
                  ),
                  "parentUTR" -> "1234567890"
                ),
                "businessAddress" -> Json.obj(
                  "line1" -> "Premises Line 1",
                  "line2" -> "Line 2",
                  "line3" -> "Locality",
                  "line4" -> "Region",
                  "postcode" -> "ZZ1 1ZZ",
                  "country" -> "Country"
                ),
                "businessContactDetails" -> Json.obj(
                  "phoneNumber" -> "02072899066",
                  "mobileNumber" -> "07567293726",
                  "email" -> "test@email.co.uk"
                )
              )
            )
          )

        res mustBe expectedJsonBody
      }

      "registration is in Draft status, at Handoff 5-1, sending takeovers block" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        val confRefsWithoutPayment: ConfirmationReferences = ConfirmationReferences(
          acknowledgementReference = ackRef,
          transactionId = transId,
          paymentReference = None,
          paymentAmount = None
        )

        val validTakeover: Option[TakeoverDetails] = Some(TakeoverDetails(
          replacingAnotherBusiness = true,
          businessName = Some("Takeover name"),
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


        ctRepository.insert(
          draftRegistration.copy(
            companyDetails =
              Some(CompanyDetails(
                companyName = "testCompanyName",
                CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("ZZ1 1ZZ"), Some("Region")),
                PPOB("RO", None),
                jurisdiction = "testJurisdiction"
              )), takeoverDetails = validTakeover
          ))


        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 200, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs()))
        response.status mustBe 200
        response.json mustBe Json.toJson(confRefsWithoutPayment)

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithoutPayment)
        reg.sessionIdentifiers mustBe None
        reg.status mustBe HELD

        val requestBody: JsObject = getPOSTRequestJsonBody(ctSubmissionUrl).as[JsObject]
        val registration: JsObject = (requestBody \ "registration" \ "metadata").as[JsObject] - "sessionId" - "formCreationTimestamp"
        val corp: JsObject = (requestBody \ "registration" \ "corporationTax").as[JsObject]
        val res: JsObject = requestBody.as[JsObject] - "registration" ++ Json.obj("registration" -> Json.obj("metadata" -> registration, "corporationTax" -> corp))

        val expectedJsonBody: JsObject = Json.obj(
          "acknowledgementReference" -> "BRCT00000000001",
          "registration" -> Json.obj(
            "metadata" -> Json.obj(
              "businessType" -> "Limited company",
              "submissionFromAgent" -> false,
              "declareAccurateAndComplete" -> true,
              "credentialId" -> "testAuthProviderId",
              "language" -> "en",
              "completionCapacity" -> "Director"
            ),
            "corporationTax" -> Json.obj(
              "companyOfficeNumber" -> "623",
              "hasCompanyTakenOverBusiness" -> true,
              "businessTakeOverDetails" -> Json.obj(
                "businessNameLine1" -> "Takeover name",
                "businessTakeoverAddress" -> Json.obj(
                  "line1" -> "Takeover 1",
                  "line2" -> "Takeover 2",
                  "line3" -> "TTelford",
                  "line4" -> "TShropshire",
                  "postcode" -> "TO1 1ZZ",
                  "country" -> "UK"
                ),
                "prevOwnersName" -> "prev name",
                "prevOwnerAddress" -> Json.obj(
                  "line1" -> "Prev 1",
                  "line2" -> "Prev 2",
                  "line3" -> "PTelford",
                  "line4" -> "PShropshire",
                  "postcode" -> "PR1 1ZZ",
                  "country" -> "UK"
                )
              ),
              "companiesHouseCompanyName" -> "testCompanyName",
              "returnsOnCT61" -> true,
              "companyACharity" -> false,
              "companyMemberOfGroup" -> false,
              "businessAddress" -> Json.obj(
                "line1" -> "Premises Line 1",
                "line2" -> "Line 2",
                "line3" -> "Locality",
                "line4" -> "Region",
                "postcode" -> "ZZ1 1ZZ",
                "country" -> "Country"
              ),
              "businessContactDetails" -> Json.obj(
                "phoneNumber" -> "02072899066",
                "mobileNumber" -> "07567293726",
                "email" -> "test@email.co.uk"
              )
            )
          )
        )

        res mustBe expectedJsonBody
      }

      "registration is in Draft status, at Handoff 5-1, sending empty takeovers block" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        val confRefsWithoutPayment: ConfirmationReferences = ConfirmationReferences(
          acknowledgementReference = ackRef,
          transactionId = transId,
          paymentReference = None,
          paymentAmount = None
        )

        val validTakeover: Option[TakeoverDetails] = Some(TakeoverDetails(
          replacingAnotherBusiness = false,
          businessName = None,
          businessTakeoverAddress = None,
          prevOwnersName = None,
          prevOwnersAddress = None
        ))

        ctRepository.insert(
          draftRegistration.copy(
            companyDetails =
              Some(CompanyDetails(
                companyName = "testCompanyName",
                CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("ZZ1 1ZZ"), Some("Region")),
                PPOB("RO", None),
                jurisdiction = "testJurisdiction"
              )), takeoverDetails = validTakeover
          ))


        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 200, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs()))
        response.status mustBe 200
        response.json mustBe Json.toJson(confRefsWithoutPayment)

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithoutPayment)
        reg.sessionIdentifiers mustBe None
        reg.status mustBe HELD

        val requestBody: JsObject = getPOSTRequestJsonBody(ctSubmissionUrl).as[JsObject]
        val registration: JsObject = (requestBody \ "registration" \ "metadata").as[JsObject] - "sessionId" - "formCreationTimestamp"
        val corp: JsObject = (requestBody \ "registration" \ "corporationTax").as[JsObject]
        val res: JsObject = requestBody.as[JsObject] - "registration" ++ Json.obj("registration" -> Json.obj("metadata" -> registration, "corporationTax" -> corp))

        val expectedJsonBody: JsObject = Json.obj(
          "acknowledgementReference" -> "BRCT00000000001",
          "registration" -> Json.obj(
            "metadata" -> Json.obj(
              "businessType" -> "Limited company",
              "submissionFromAgent" -> false,
              "declareAccurateAndComplete" -> true,
              "credentialId" -> "testAuthProviderId",
              "language" -> "en",
              "completionCapacity" -> "Director"
            ),
            "corporationTax" -> Json.obj(
              "companyOfficeNumber" -> "623",
              "hasCompanyTakenOverBusiness" -> false,
              "companiesHouseCompanyName" -> "testCompanyName",
              "returnsOnCT61" -> true,
              "companyACharity" -> false,
              "companyMemberOfGroup" -> false,
              "businessAddress" -> Json.obj(
                "line1" -> "Premises Line 1",
                "line2" -> "Line 2",
                "line3" -> "Locality",
                "line4" -> "Region",
                "postcode" -> "ZZ1 1ZZ",
                "country" -> "Country"
              ),
              "businessContactDetails" -> Json.obj(
                "phoneNumber" -> "02072899066",
                "mobileNumber" -> "07567293726",
                "email" -> "test@email.co.uk"
              )
            )
          )
        )

        res mustBe expectedJsonBody
      }

      "registration is in Draft status and update Confirmation References with Ack Ref but API submission FAILED 403 (new HO5-1)" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        val confRefsWithoutPayment: ConfirmationReferences = ConfirmationReferences(
          acknowledgementReference = ackRef,
          transactionId = transId,
          paymentReference = None,
          paymentAmount = None
        )

        ctRepository.insert(draftRegistration)

        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 403, "")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs()))
        response.status mustBe 400

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithoutPayment)
        reg.sessionIdentifiers mustBe Some(SessionIds(SessionId, authProviderId))
        reg.status mustBe RegistrationStatus.LOCKED
      }

      "registration is in Draft status and update Confirmation References with Ack Ref but API submission FAILED 429 (new HO5-1)" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        val confRefsWithoutPayment: ConfirmationReferences = ConfirmationReferences(
          acknowledgementReference = ackRef,
          transactionId = transId,
          paymentReference = None,
          paymentAmount = None
        )

        ctRepository.insert(draftRegistration)

        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 429, "")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs()))
        response.status mustBe 503

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithoutPayment)
        reg.sessionIdentifiers mustBe Some(SessionIds(SessionId, authProviderId))
        reg.status mustBe RegistrationStatus.LOCKED
      }

      "registration is in Locked status and update Confirmation References with Payment infos (new HO6)" in new Setup {
        stubAuthorise(200, authorisedRetrievals)

        val confRefsWithoutPayment: ConfirmationReferences = ConfirmationReferences(
          acknowledgementReference = ackRef,
          transactionId = transId,
          paymentReference = None,
          paymentAmount = None
        )
        val lockedRegistration: CorporationTaxRegistration = draftRegistration.copy(status = RegistrationStatus.LOCKED, confirmationReferences = Some(confRefsWithoutPayment))

        ctRepository.insert(lockedRegistration)

        stubGet(s"/business-registration/business-tax-registration/$regId", 200, businessRegistrationResponse)
        stubPost(ctSubmissionUrl, 200, """{"a": "b"}""")
        stubFor(post(urlEqualTo("/incorporation-information/subscribe/trans-id-2345/regime/ctax/subscriber/SCRS?force=true"))
          .willReturn(
            aResponse().
              withStatus(202).
              withBody("""{"a": "b"}""")
          )
        )

        val response: WSResponse = await(client(s"/$regId/confirmation-references").put(jsonConfirmationRefs(ackRef, Some(payRef), Some(payAmount))))
        response.status mustBe 200
        response.json mustBe Json.toJson(confRefsWithPayment)

        val reg: CorporationTaxRegistration = ctRepository.findAll.head
        reg.confirmationReferences mustBe Some(confRefsWithPayment)
        reg.sessionIdentifiers mustBe None
        reg.status mustBe HELD
      }

      "GET /corporation-tax-registration" must {
        "return a 200 and an unencrypted CT-UTR" in new Setup {
          stubAuthorise(200, authorisedRetrievals)

          ctRepository.insert(fullCorporationTaxRegistration)

          val ctRegJson = await(ctRepository.collection.find[JsObject]().head())

          val encryptedCtUtr: JsLookupResult = ctRegJson \ "acknowledgementReferences" \ "ct-utr"

          encryptedCtUtr.get mustBe crypto.wts.writes(ctutr)

          val response: WSResponse = await(client(s"/$regId/corporation-tax-registration").get())

          response.status mustBe 200
          (response.json \ "acknowledgementReferences" \ "ctUtr").get.as[String] mustBe ctutr
        }
      }
    }
  }
}
