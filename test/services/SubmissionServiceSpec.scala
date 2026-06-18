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

package services

import audit.SubmissionEventDetail
import cats.data.OptionT
import com.mongodb.client.result.UpdateResult
import config.LangConstants
import connectors._
import fixtures.CorporationTaxRegistrationFixture
import helpers.BaseSpec
import mocks.AuthorisationMocks
import models.RegistrationStatus._
import models._
import models.api._
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.{any, eq => eqTo}
import org.mockito.Mockito._
import org.mongodb.scala.bson.BsonDocument
import org.scalatest.concurrent.Eventually
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories._
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, SessionId, UpstreamErrorResponse}
import uk.gov.hmrc.play.audit.http.connector.AuditResult.Success
import utils.{LogCapturingHelper, PagerDutyKeys}

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class SubmissionServiceSpec extends BaseSpec with AuthorisationMocks with CorporationTaxRegistrationFixture with LogCapturingHelper with Eventually {


  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("testSessionId")))
  implicit val req: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/test-path")

  val mockBRConnector: BusinessRegistrationConnector = mock[BusinessRegistrationConnector]
  val mockCorpTaxRepo: CorporationTaxRegistrationMongoRepository = mock[CorporationTaxRegistrationMongoRepository]
  val mockSequenceRepo: SequenceMongoRepository = mock[SequenceMongoRepository]
  val mockAuditService: AuditService = mock[AuditService]
  val mockIIConnector: IncorporationInformationConnector = mock[IncorporationInformationConnector]
  val mockSubmissionEventService: SubmissionEventService = mock[SubmissionEventService]
  val mockCorpTaxService: CorporationTaxRegistrationService = mock[CorporationTaxRegistrationService]

  val dateTime: Instant = Instant.parse("2016-10-27T16:28:59.000Z")

  val regId: String = "reg-id-12345"
  val transId: String = "trans-id-12345"
  val timestamp: String = "2016-10-27T17:06:23.000Z"
  val authProviderId: String = "auth-prov-id-12345"

  override def beforeEach(): Unit = {
    reset(
      mockCorpTaxRepo, mockSequenceMongoRepository, mockAuthConnector, mockBRConnector,
      mockIncorporationCheckAPIConnector, mockAuditService, mockIIConnector, mockSubmissionEventService
    )
  }

  class Setup {
    val service: SubmissionService = new SubmissionService {
      override val cTRegistrationRepository: CorporationTaxRegistrationMongoRepository = mockCorpTaxRepo
      override val sequenceRepository: SequenceMongoRepository = mockSequenceMongoRepository
      override val incorpInfoConnector: IncorporationInformationConnector = mockIIConnector
      override val submissionEventService: SubmissionEventService = mockSubmissionEventService
      override val auditService: AuditService = mockAuditService
      override val brConnector: BusinessRegistrationConnector = mockBRConnector
      override val corpTaxRegService: CorporationTaxRegistrationService = mockCorpTaxService
      implicit val ec: ExecutionContext = global
      override def instantNow: Instant = dateTime
    }
  }

  def corporationTaxRegistration(regId: String = regId,
                                 status: String = DRAFT,
                                 confRefs: Option[ConfirmationReferences] = None): CorporationTaxRegistration = {
    CorporationTaxRegistration(
      internalId = "testID",
      registrationID = regId,
      formCreationTimestamp = dateTime.toString,
      language = LangConstants.english,
      companyDetails = Some(CompanyDetails(
        "testCompanyName",
        CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
        PPOB("MANUAL", Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), None, None, "txid"))),
        "testJurisdiction"
      )),
      contactDetails = Some(ContactDetails(
        Some("0123456789"),
        Some("0123456789"),
        Some("test@email.co.uk")
      )),
      tradingDetails = Some(TradingDetails("false")),
      status = status,
      confirmationReferences = confRefs
    )
  }

  def partialApiSubmission(ackRef: String, timestamp: String = "2016-10-27T17:06:23.000Z"): JsObject =
    Json.obj(
      "acknowledgementReference" -> s"$ackRef",
      "registration" -> Json.obj(
        "metadata" -> Json.obj(
          "businessType" -> "Limited company",
          "submissionFromAgent" -> false,
          "declareAccurateAndComplete" -> true,
          "sessionId" -> "session-40fdf8c0-e2b1-437c-83b5-8689c2e1bc43",
          "credentialId" -> "cred-id-543212311772",
          "language" -> "en",
          "formCreationTimestamp" -> s"$timestamp",
          "completionCapacity" -> "Other",
          "completionCapacityOther" -> "director"
        ),
        "corporationTax" -> Json.obj(
          "companyOfficeNumber" -> "001",
          "hasCompanyTakenOverBusiness" -> false,
          "companyMemberOfGroup" -> false,
          "companiesHouseCompanyName" -> "testCompanyName",
          "returnsOnCT61" -> false,
          "companyACharity" -> false,
          "businessAddress" -> Json.obj(
            "line1" -> "",
            "line2" -> "",
            "line3" -> "",
            "line4" -> "",
            "postcode" -> "",
            "country" -> ""
          ),
          "businessContactDetails" -> Json.obj(
            "telephoneNumber" -> "123",
            "mobileNumber" -> "123",
            "emailAddress" -> "6email@whatever.com"
          )
        )
      )
    )

  "handleSubmission" must {

    val ho6RequestBody = ConfirmationReferences("", "testPayRef", Some("testPayAmount"), Some("12"))

    "handle the partial submission when the document is not yet Held" in new Setup {

      val confRefs: ConfirmationReferences = ho6RequestBody
      val businessRegistration: BusinessRegistration = BusinessRegistration(
        regId,
        "testTimeStamp",
        "en",
        Some("Director")
      )

      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(regId))))
        .thenReturn(Future.successful(Option(corporationTaxRegistration(regId, DRAFT, Some(confRefs)))))

      when(mockCorpTaxRepo.retrieveConfirmationReferences(eqTo(regId)))
        .thenReturn(Future.successful(Some(confRefs)))

      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))

      when(mockIIConnector.registerInterest(eqTo(regId), any(), any())(any(), any()))
        .thenReturn(Future.successful(true))

      when(mockSubmissionEventService.ctSubmission(any(), any(), eqTo(regId))(any()))
        .thenReturn(Future.successful(HttpResponse(202, "")))

      when(mockCorpTaxRepo.retrieveCompanyDetails(eqTo(regId)))
        .thenReturn(Future.successful(Some(CompanyDetails("", CHROAddress("", "", None, "", "", None, None, None), PPOB("", None), ""))))

      when(mockCorpTaxRepo.updateRegistrationToHeld(eqTo(regId), eqTo(confRefs)))
        .thenReturn(Future.successful(Some(corporationTaxRegistration(regId, transId, Option(confRefs)))))

      val result: ConfirmationReferences = await(service.handleSubmission(regId, authProviderId, ho6RequestBody, isAdmin = false))
      result mustBe confRefs
    }

    "return the confirmation references when document is already Held and no pager duty because txIds match" in new Setup {

      val confRefs: ConfirmationReferences = ConfirmationReferences("", "testPayRef", Some("testPayAmount"), Some("12"))

      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(regId))))
        .thenReturn(Future.successful(Option(corporationTaxRegistration(regId, HELD, Some(confRefs)))))

      when(mockCorpTaxRepo.retrieveConfirmationReferences(eqTo(regId)))
        .thenReturn(Future.successful(Some(confRefs)))
      withCaptureOfLoggingFrom(service.logger) { logEvents =>
        await(service.handleSubmission(regId, authProviderId, ho6RequestBody, isAdmin = false)) mustBe confRefs
        logEvents.count(_.getMessage.contains(s"${PagerDutyKeys.TXID_IN_CR_DOESNT_MATCH_HANDOFF_TXID}")) mustBe 0
      }
    }

    "throw pager duty if txId in handOff doesnt match txId in CR and status is already Held" in new Setup {

      val confRefs: ConfirmationReferences = ConfirmationReferences("testAckRef", "testTransactionId", Some("testPayAmount"), Some("12"))

      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(regId))))
        .thenReturn(Future.successful(Option(corporationTaxRegistration(regId, HELD, Some(confRefs)))))

      when(mockCorpTaxRepo.retrieveConfirmationReferences(eqTo(regId)))
        .thenReturn(Future.successful(Some(confRefs)))
      withCaptureOfLoggingFrom(service.logger) { logEvents =>

        await(service.handleSubmission(regId, authProviderId, ho6RequestBody, isAdmin = false)) mustBe confRefs
        logEvents.count(_.getMessage.contains(s"${PagerDutyKeys.TXID_IN_CR_DOESNT_MATCH_HANDOFF_TXID}")) mustBe 1
      }
    }

    "return the confirmation references when document is already Held and update it with payment info" in new Setup {

      val backendRefs: ConfirmationReferences = ho6RequestBody.copy(acknowledgementReference = "BRCT00000000123", paymentReference = None, paymentAmount = None)
      val confRefs: ConfirmationReferences = ConfirmationReferences("BRCT00000000123", "testPayRef", Some("testPayAmount"), Some("12"))

      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(regId))))
        .thenReturn(Future.successful(Option(corporationTaxRegistration(regId, HELD, Some(confRefs)))))

      when(mockCorpTaxRepo.retrieveConfirmationReferences(eqTo(regId)))
        .thenReturn(Future.successful(Some(backendRefs)))

      when(mockCorpTaxRepo.updateConfirmationReferences(eqTo(regId), eqTo(confRefs)))
        .thenReturn(Future.successful(Some(confRefs)))

      await(service.handleSubmission(regId, authProviderId, confRefs, isAdmin = false)) mustBe confRefs
    }

    "throw an exception when document is in Held but has no confirmation references" in new Setup {
      when(mockCorpTaxRepo.fetchDocumentStatus(eqTo(regId)))
        .thenReturn(OptionT(Future.successful(Option(HELD))))
      when(mockCorpTaxRepo.retrieveConfirmationReferences(eqTo(regId)))
        .thenReturn(Future.successful(None))

      intercept[RuntimeException](await(service.handleSubmission(regId, authProviderId, ho6RequestBody, isAdmin = false)))
    }
  }

  "storeConfirmationReferencesAndUpdateStatus" must {

    val confRefs: ConfirmationReferences = ConfirmationReferences("testAckRef", "testPayRef", Some("testPayAmount"), Some("12"))

    "return the same confirmation refs that were supplied on a successful store with NO status" in new Setup {

      when(mockCorpTaxRepo.updateConfirmationReferences(eqTo(regId), eqTo(confRefs)))
        .thenReturn(Future.successful(Some(confRefs)))

      val result: ConfirmationReferences = await(service.storeConfirmationReferencesAndUpdateStatus(regId, confRefs, None))
      result mustBe confRefs
    }

    "return the same confirmation references that were supplied on a successful store with a status" in new Setup {
      when(mockCorpTaxRepo.updateConfirmationReferencesAndUpdateStatus(eqTo(regId), eqTo(confRefs), eqTo("locked")))
        .thenReturn(Future.successful(Some(confRefs)))
      val result: ConfirmationReferences = await(service.storeConfirmationReferencesAndUpdateStatus(regId, confRefs, Some(RegistrationStatus.LOCKED)))
      result mustBe confRefs
    }

    "throw a RuntimeException when the repository returns a None on an unsuccessful store" in new Setup {

      when(mockCorpTaxRepo.updateConfirmationReferences(eqTo(regId), eqTo(confRefs)))
        .thenReturn(Future.successful(None))

      val ex: Exception = intercept[RuntimeException](await(service.storeConfirmationReferencesAndUpdateStatus(regId, confRefs, None)))
      ex.getMessage mustBe s"[HO6] Could not update confirmation refs for regId: $regId - registration document not found"
    }
  }

  "storePartialSubmission" must {

    val ackRef: String = "ack-ref-12345"

    val partialSubmission: JsObject = partialApiSubmission(ackRef)

    "send a partial submission to API when the gateway feature flag is enabled and return a HeldSubmissionData object when the connector returns a 200 HttpResponse" in new Setup {
      when(mockSubmissionEventService.ctSubmission(eqTo(ackRef), eqTo(partialSubmission), eqTo(regId))(any()))
        .thenReturn(Future.successful(HttpResponse(200, "")))

      val result: HttpResponse = await(service.submitPartialToApi(regId, ackRef, partialSubmission, authProviderId))

      result.status mustBe 200

      verify(mockSubmissionEventService, times(1)).ctSubmission(eqTo(ackRef), eqTo(partialSubmission), eqTo(regId))(any())
    }

    "throw a Runtime exception, save sessionID/credID when the API feature flag is enabled and submission API to fails on a 400" in new Setup {
      when(mockSubmissionEventService.ctSubmission(eqTo(ackRef), eqTo(partialSubmission), eqTo(regId))(any()))
        .thenReturn(Future.failed(UpstreamErrorResponse("fail", 400, 400)))
      when(mockCorpTaxRepo.storeSessionIdentifiers(eqTo(regId), any(), any()))
        .thenReturn(Future.successful(true))

      intercept[UpstreamErrorResponse](await(service.submitPartialToApi(regId, ackRef, partialSubmission, authProviderId)))

      verify(mockSubmissionEventService, times(1)).ctSubmission(eqTo(ackRef), eqTo(partialSubmission), eqTo(regId))(any())
    }

    "throw a Runtime exception, save sessionID/credID when the API feature flag is enabled and submission API to fails on a 500" in new Setup {
      when(mockSubmissionEventService.ctSubmission(eqTo(ackRef), eqTo(partialSubmission), eqTo(regId))(any()))
        .thenReturn(Future.failed(UpstreamErrorResponse("fail", 500, 500)))
      when(mockCorpTaxRepo.storeSessionIdentifiers(eqTo(regId), any(), any()))
        .thenReturn(Future.successful(true))

      intercept[UpstreamErrorResponse](await(service.submitPartialToApi(regId, ackRef, partialSubmission, authProviderId)))

      verify(mockSubmissionEventService, times(1)).ctSubmission(eqTo(ackRef), eqTo(partialSubmission), eqTo(regId))(any())
    }
  }

  "retrieveBRMetadata" must {

    val businessRegistration = BusinessRegistration(
      regId,
      "testTimeStamp",
      "en",
      Some("Director")
    )

    "return a business registration" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))

      val result: BusinessRegistration = await(service.retrieveBRMetadata(regId))
      result mustBe businessRegistration
    }

    "return future failed if success response but regId's do not match" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))

      intercept[Exception](await(service.retrieveBRMetadata("123DoesNotMatch")))
    }

    "return future failed if anything but success response" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationNotFoundResponse))

      intercept[Exception](await(service.retrieveBRMetadata(regId)))
    }
  }

  "Build partial API submission" must {

    val registrationId = "testRegId"
    val ackRef = "testAckRef"
    val sessionId = "testSessionId"
    val credId = "testCredId"

    val businessRegistration = BusinessRegistration(
      regId,
      dateTime.toString,
      "en",
      Some("Director")
    )

    val companyDetails1 = CompanyDetails(
      "name",
      CHROAddress("P", "1", Some("2"), "C", "L", Some("PO"), Some("PC"), Some("R")),
      PPOB("MANUAL", Some(PPOBAddress("1", "2", Some("3"), Some("4"), Some("ZZ1 1ZZ"), Some("C"), None, "txid"))),
      "J"
    )

    val companyDetails2 = CompanyDetails(
      "name",
      CHROAddress("P", "1", Some("2"), "C", "L", Some("PO"), Some("PC"), Some("R")),
      PPOB("MANUAL", Some(PPOBAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None, None, "txid"))),
      "J"
    )

    val companyDetails3 = CompanyDetails(
      "name",
      CHROAddress("P", "1", Some("2"), "CustomCountry", "L", Some("PO"), Some("ZZ1 1ZZ"), Some("R")),
      PPOB("RO", None),
      "J"
    )

    val companyDetailsWithIllegalChars = CompanyDetails(
      "name",
      CHROAddress("P", "1:", Some("2;"), "CustomCountry", "L", Some("PO"), Some("ZZ1 1ZZ"), Some("R")),
      PPOB("RO", None),
      "J"
    )

    val contactDetails1 = ContactDetails(Some("1"), Some("2"), Some("a@b.c"))
    val contactDetails2 = ContactDetails(None, None, Some("a@b.c"))

    def getCTReg(regId: String, company: Option[CompanyDetails], contact: Option[ContactDetails], optTakeovers: Option[TakeoverDetails] = None): CorporationTaxRegistration =
      CorporationTaxRegistration(
        internalId = "testID",
        registrationID = regId,
        formCreationTimestamp = dateTime.toString,
        language = LangConstants.english,
        companyDetails = company,
        contactDetails = contact,
        tradingDetails = Some(TradingDetails("false")),
        takeoverDetails = optTakeovers
      )

    val corporationTaxRegistration = CorporationTaxRegistration(
      internalId = "testID",
      registrationID = registrationId,
      formCreationTimestamp = dateTime.toString,
      language = LangConstants.english,
      companyDetails = Some(CompanyDetails(
        "testCompanyName",
        CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
        PPOB("MANUAL", Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"), None, "txid"))),
        "testJurisdiction"
      )),
      contactDetails = Some(ContactDetails(
        Some("0123456789"),
        Some("0123456789"),
        Some("test@email.co.uk")
      )),
      tradingDetails = Some(TradingDetails("false"))
    )

    "return a valid partial API submission" in new Setup {
      implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("testSessionId")))

      val interimApiRegistration: InterimApiRegistration = InterimApiRegistration(
        ackRef,
        Metadata(sessionId, authProviderId, "en", dateTime, Director),
        InterimCorporationTax(
          corporationTaxRegistration.companyDetails.get.companyName,
          returnsOnCT61 = false,
          Some(BusinessAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"))),
          BusinessContactDetails(Some("0123456789"), Some("0123456789"), Some("test@email.co.uk"))
        )
      )
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val result: InterimApiRegistration = service.buildPartialApiSubmission(regId, ackRef, authProviderId, businessRegistration, corporationTaxRegistration)
      result.toString mustBe interimApiRegistration.toString
    }

    "return a valid InterimApiRegistration with full contact details" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val ctReg: CorporationTaxRegistration = getCTReg(regId, Some(companyDetails1), Some(contactDetails1))
      val result: InterimApiRegistration = service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg)

      result mustBe InterimApiRegistration(
        ackRef,
        Metadata(sessionId, credId, "en", dateTime, Director),
        InterimCorporationTax(
          "name",
          returnsOnCT61 = false,
          Some(BusinessAddress("1", "2", Some("3"), Some("4"), Some("ZZ1 1ZZ"), Some("C"))),
          BusinessContactDetails(Some("1"), Some("2"), Some("a@b.c"))
        )
      )
    }

    "return a valid InterimApiRegistration with minimal details" in new Setup {

      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val ctReg: CorporationTaxRegistration = getCTReg(regId, Some(companyDetails2), Some(contactDetails2))
      val result: InterimApiRegistration = service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg)

      result mustBe InterimApiRegistration(
        ackRef,
        Metadata(sessionId, credId, "en", dateTime, Director),
        InterimCorporationTax(
          "name",
          returnsOnCT61 = false,
          Some(BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None)),
          BusinessContactDetails(None, None, Some("a@b.c")),
          groups = None
        )
      )
    }

    "return a valid InterimApiRegistration with RO address as the PPOB" in new Setup {

      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      when(mockCorpTaxService.convertROToPPOBAddress(any()))
        .thenReturn(Some(PPOBAddress("P 1", "2", Some("L"), Some("R"), Some("ZZ1 1ZZ"), Some("CustomCountry"), None, "")))

      val ctReg: CorporationTaxRegistration = getCTReg(regId, Some(companyDetails3), Some(contactDetails2))
      val result: InterimApiRegistration = service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg)

      result mustBe InterimApiRegistration(
        ackRef,
        Metadata(sessionId, credId, "en", dateTime, Director),
        InterimCorporationTax(
          "name",
          returnsOnCT61 = false,
          Some(BusinessAddress("P 1", "2", Some("L"), Some("R"), Some("ZZ1 1ZZ"), Some("CustomCountry"))),
          BusinessContactDetails(None, None, Some("a@b.c"))
        )
      )
    }

    "return a valid InterimApiRegistration and transpose any illegal address chars with RO address as the PPOB" in new Setup {

      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      when(mockCorpTaxService.convertROToPPOBAddress(any()))
        .thenReturn(Some(PPOBAddress("P 1\\", "2:;", Some("L\\"), Some("R;:"), Some("ZZ1 1ZZ"), Some("CustomCountry"), None, "")))

      val ctReg: CorporationTaxRegistration = getCTReg(regId, Some(companyDetailsWithIllegalChars), Some(contactDetails2))
      val result: InterimApiRegistration = service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg)

      result mustBe InterimApiRegistration(
        ackRef,
        Metadata(sessionId, credId, "en", dateTime, Director),
        InterimCorporationTax(
          "name",
          returnsOnCT61 = false,
          Some(BusinessAddress("P 1/", "2.,", Some("L/"), Some("R,."), Some("ZZ1 1ZZ"), Some("CustomCountry"))),
          BusinessContactDetails(None, None, Some("a@b.c"))
        )
      )
    }

    "return a valid partial API submission if the sessionID is available in mongo" in new Setup {
      implicit val hc: HeaderCarrier = HeaderCarrier()

      val interimApiRegistration: InterimApiRegistration = InterimApiRegistration(
        ackRef,
        Metadata(sessionId, authProviderId, "en", dateTime, Director),
        InterimCorporationTax(
          corporationTaxRegistration.companyDetails.get.companyName,
          returnsOnCT61 = false,
          Some(BusinessAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"))),
          BusinessContactDetails(Some("0123456789"), Some("0123456789"), Some("test@email.co.uk"))
        )
      )

      when(mockBRConnector.adminRetrieveMetadata(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val result: InterimApiRegistration = service.buildPartialApiSubmission(
        regId, ackRef, authProviderId, businessRegistration, corporationTaxRegistration.copy(sessionIdentifiers = Some(SessionIds(sessionId, authProviderId)))
      )
      result.toString mustBe interimApiRegistration.toString
    }

    "return a valid API Submission if groups is provided but relief is false" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val ctReg: CorporationTaxRegistration = getCTReg(regId, Some(companyDetails2), Some(contactDetails2)).copy(groups = Some(Groups(groupRelief = false, None, None, None)))
      val result: InterimApiRegistration = service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg)

      result mustBe InterimApiRegistration(
        ackRef,
        Metadata(sessionId, credId, "en", dateTime, Director),
        InterimCorporationTax(
          "name",
          returnsOnCT61 = false,
          Some(BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None)),
          BusinessContactDetails(None, None, Some("a@b.c")),
          groups = Some(Groups(groupRelief = false, None, None, None))
        )
      )
    }

    "return a valid API submission if groups is provided but relief is false and data is provided - setting this data to None" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val ctReg: CorporationTaxRegistration =
        getCTReg(
          regId,
          Some(companyDetails2),
          Some(contactDetails2)).copy(
          groups = Some(Groups(groupRelief = false, Some(GroupCompanyName("testGroupName", GroupCompanyNameEnum.Other)), None, None))
        )

      val result: InterimApiRegistration = service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg)

      result mustBe InterimApiRegistration(
        ackRef,
        Metadata(sessionId, credId, "en", dateTime, Director),
        InterimCorporationTax(
          "name",
          returnsOnCT61 = false,
          Some(BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None)),
          BusinessContactDetails(None, None, Some("a@b.c")),
          groups = Some(Groups(groupRelief = false, None, None, None))
        ))
    }

    "return a valid api submission if groups is provided but name needs normalising" in new Setup {

      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val ctReg: CorporationTaxRegistration =
        getCTReg(
          regId,
          Some(companyDetails2),
          Some(contactDetails2)).copy(groups =
          Some(Groups(
            groupRelief = true,
            Some(GroupCompanyName("%%% This is my compa$y over 20 chars and has special chars at the start", GroupCompanyNameEnum.Other)),
            Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF, BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None))),
            Some(GroupUTR(None))
          ))
        )
      val result: InterimApiRegistration = service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg)

      result mustBe InterimApiRegistration(
        ackRef,
        Metadata(sessionId, credId, "en", dateTime, Director),
        InterimCorporationTax(
          "name",
          returnsOnCT61 = false,
          Some(BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None)),
          BusinessContactDetails(None, None, Some("a@b.c")),
          groups = Some(
            Groups(
              groupRelief = true,
              Some(GroupCompanyName(" This is my compay o", GroupCompanyNameEnum.Other)),
              Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF, BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None))), Some(GroupUTR(None)))
          )))
    }

    "return a valid api submission if groups is provided but there are illegal characters in the address supplied by Coho" in new Setup {

      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val ctReg: CorporationTaxRegistration =
        getCTReg(
          regId,
          Some(companyDetails2),
          Some(contactDetails2)).copy(groups =
          Some(Groups(
            groupRelief = true,
            Some(GroupCompanyName("%%% This is my compa$y over 20 chars and has special chars at the start", GroupCompanyNameEnum.Other)),
            Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF, BusinessAddress("line 1:", "line 2:", Some("line 3:"),Some("line 4:"), Some("ZZ1 1ZZ"), None))),
            Some(GroupUTR(None))
          ))
        )
      val result: InterimApiRegistration = service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg)

      result mustBe InterimApiRegistration(
        ackRef,
        Metadata(sessionId, credId, "en", dateTime, Director),
        InterimCorporationTax(
          "name",
          returnsOnCT61 = false,
          Some(BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None)),
          BusinessContactDetails(None, None, Some("a@b.c")),
          groups = Some(
            Groups(
              groupRelief = true,
              Some(GroupCompanyName(" This is my compay o", GroupCompanyNameEnum.Other)),
              Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF, BusinessAddress("line 1.", "line 2.", Some("line 3."), Some("line 4."), Some("ZZ1 1ZZ"), None))), Some(GroupUTR(None)))
          )))
    }

    "return a valid api submission if takeover block is provided" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val testAddress: Address = Address(
        line1 = "line1",
        line2 = "line2",
        line3 = Some("line3"),
        line4 = Some("line4"),
        postcode = Some("aa11aa"),
        country = Some("UK")
      )

      val anotherTestAddress: Address = Address(
        line1 = "anotherLine1",
        line2 = "anotherLine2",
        line3 = Some("anotherLine3"),
        line4 = Some("anotherLine4"),
        postcode = Some("bb11bb"),
        country = Some("UK")
      )

      val ctReg: CorporationTaxRegistration =
        getCTReg(
          regId,
          Some(companyDetails2),
          Some(contactDetails2)).copy(
          takeoverDetails =
            Some(TakeoverDetails(
              replacingAnotherBusiness = true,
              businessName = Some("testBusinessName"),
              businessTakeoverAddress = Some(testAddress),
              prevOwnersName = Some("previousOwnerName"),
              prevOwnersAddress = Some(anotherTestAddress)
            ))
        )

      val result: InterimApiRegistration = service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg)

      result mustBe InterimApiRegistration(
        ackRef,
        Metadata(sessionId, credId, "en", dateTime, Director),
        InterimCorporationTax(
          "name",
          returnsOnCT61 = false,
          Some(BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None)),
          BusinessContactDetails(None, None, Some("a@b.c")),
          takeOver =
            Some(TakeoverDetails(
              replacingAnotherBusiness = true,
              businessName = Some("testBusinessName"),
              businessTakeoverAddress = Some(testAddress),
              prevOwnersName = Some("previousOwnerName"),
              prevOwnersAddress = Some(anotherTestAddress)
            ))
        ))
    }


    "return a valid api submission if takeover block is provided with illegal characters in the previous business and owner names" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val testAddress: Address = Address(
        line1 = "line1",
        line2 = "line2",
        line3 = Some("line3"),
        line4 = Some("line4"),
        postcode = Some("aa11aa"),
        country = Some("UK")
      )

      val anotherTestAddress: Address = Address(
        line1 = "anotherLine1",
        line2 = "anotherLine2",
        line3 = Some("anotherLine3"),
        line4 = Some("anotherLine4"),
        postcode = Some("bb11bb"),
        country = Some("UK")
      )

      val ctReg: CorporationTaxRegistration =
        getCTReg(
          regId,
          Some(companyDetails2),
          Some(contactDetails2)).copy(
          takeoverDetails =
            Some(TakeoverDetails(
              replacingAnotherBusiness = true,
              businessName = Some("testBusinessNameæ\t\tf"),
              businessTakeoverAddress = Some(testAddress),
              prevOwnersName = Some("previousOwnerName\t\t"),
              prevOwnersAddress = Some(anotherTestAddress)
            ))
        )

      val result: InterimApiRegistration = service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg)

      result mustBe InterimApiRegistration(
        ackRef,
        Metadata(sessionId, credId, "en", dateTime, Director),
        InterimCorporationTax(
          "name",
          returnsOnCT61 = false,
          Some(BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None)),
          BusinessContactDetails(None, None, Some("a@b.c")),
          takeOver =
            Some(TakeoverDetails(
              replacingAnotherBusiness = true,
              businessName = Some("testBusinessNameaef"),
              businessTakeoverAddress = Some(testAddress),
              prevOwnersName = Some("previousOwnerName"),
              prevOwnersAddress = Some(anotherTestAddress)
            ))
        ))
    }






    "return a valid API Submission if takeovers is false" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val ctReg: CorporationTaxRegistration =
        getCTReg(
          regId,
          Some(companyDetails2),
          Some(contactDetails2)).copy(
          groups = Some(Groups(groupRelief = false, None, None, None)),
          takeoverDetails = Some(TakeoverDetails(replacingAnotherBusiness = false, None, None, None, None)))
      val result: InterimApiRegistration = service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg)

      result mustBe InterimApiRegistration(
        ackRef,
        Metadata(sessionId, credId, "en", dateTime, Director),
        InterimCorporationTax(
          "name",
          returnsOnCT61 = false,
          Some(BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None)),
          BusinessContactDetails(None, None, Some("a@b.c")),
          groups = Some(Groups(groupRelief = false, None, None, None)),
          takeOver = Some(TakeoverDetails(replacingAnotherBusiness = false, None, None, None, None))
        )
      )
    }

    "throw a RuntimeException if there is no name in group block but relief is true" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val ctReg: CorporationTaxRegistration = getCTReg(regId, Some(companyDetails2), Some(contactDetails2)).copy(
        groups = Some(Groups(groupRelief = true, None, Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF, BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None))), Some(GroupUTR(None)))))

      val res: RuntimeException = intercept[RuntimeException](service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg))
      res.getMessage mustBe s"formatGroupsForSubmission groups exists but name does not: $regId"
    }

    "throw a RuntimeException if there is no address in group block but relief is true" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val ctReg: CorporationTaxRegistration = getCTReg(regId, Some(companyDetails2), Some(contactDetails2)).copy(
        groups = Some(Groups(groupRelief = true, Some(GroupCompanyName("testGroupName", GroupCompanyNameEnum.Other)), None, Some(GroupUTR(None)))))

      val res: RuntimeException = intercept[RuntimeException](service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg))
      res.getMessage mustBe s"formatGroupsForSubmission groups exists but address does not: $regId"
    }

    "throw a RuntimeException if there is no utr block in group block but relief is true" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val ctReg: CorporationTaxRegistration = getCTReg(
        regId,
        Some(companyDetails2),
        Some(contactDetails2)).copy(groups =
        Some(Groups(
          groupRelief = true,
          Some(GroupCompanyName("testGroupName", GroupCompanyNameEnum.Other)),
          Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF, BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None))),
          None
        ))
      )

      val res: RuntimeException = intercept[RuntimeException](service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg))
      res.getMessage mustBe s"formatGroupsForSubmission groups exists but utr block does not: $regId"
    }

    "throw a RuntimeException if all blocks are there but name is invalid" in new Setup {
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val ctReg: CorporationTaxRegistration =
        getCTReg(
          regId,
          Some(companyDetails2),
          Some(contactDetails2)).copy(
          groups = Some(Groups(
            groupRelief = true,
            Some(GroupCompanyName("%%&&&&&", GroupCompanyNameEnum.Other)),
            Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF, BusinessAddress("1", "1", None, None, Some("ZZ1 1ZZ"), None))), Some(GroupUTR(None)))))

      val res: RuntimeException = intercept[RuntimeException](service.buildPartialApiSubmission(regId, ackRef, credId, businessRegistration, ctReg))
      res.getMessage mustBe s"Parent group name saved does not pass Api validation: $regId"
    }

    "throw a RunTime exception if there is no sessionID in the header carrier or mongo" in new Setup {
      implicit val hc: HeaderCarrier = HeaderCarrier()

      val interimApiRegistration: InterimApiRegistration = InterimApiRegistration(
        ackRef,
        Metadata(sessionId, authProviderId, "en", dateTime, Director),
        InterimCorporationTax(
          corporationTaxRegistration.companyDetails.get.companyName,
          returnsOnCT61 = false,
          Some(BusinessAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"))),
          BusinessContactDetails(Some("0123456789"), Some("0123456789"), Some("test@email.co.uk"))
        )
      )

      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.regIDSelector(eqTo(registrationId))))
        .thenReturn(Future.successful(Some(corporationTaxRegistration)))

      val ctReg: CorporationTaxRegistration = getCTReg(regId, Some(companyDetails1), Some(contactDetails1))
      intercept[RuntimeException](service.buildPartialApiSubmission(regId, ackRef, authProviderId, businessRegistration, ctReg))
    }
  }

  "updateCTRecordWithAckRefs" must {

    val ackRef: String = "testAckRef"

    val refs: AcknowledgementReferences = AcknowledgementReferences(Option("aaa"), "bbb", "ccc")

    val updated: CorporationTaxRegistration = validHeldCorporationTaxRegistration.copy(acknowledgementReferences = Some(refs), status = RegistrationStatus.ACKNOWLEDGED)

    "return None" when {
      "the given ack ref cant be matched against a CT record" in new Setup {
        when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.ackRefSelector(eqTo(ackRef))))
          .thenReturn(Future.successful(None))

        val result: Option[CorporationTaxRegistration] = await(service.updateCTRecordWithAckRefs(ackRef, refs))
        result mustBe None
      }
    }

    "return an optional ack ref payload" when {
      "the ct record has been found and subsequently updated" in new Setup {
        when(mockCorpTaxRepo.findOneBySelector(mockCorpTaxRepo.ackRefSelector(eqTo(ackRef))))
          .thenReturn(Future.successful(Some(validHeldCorporationTaxRegistration)))

        when(mockCorpTaxRepo.updateCTRecordWithAcknowledgments(eqTo(ackRef), eqTo(updated)))
          .thenReturn(Future.successful(UpdateResult.acknowledged(1, 1, BsonDocument())))

        val result: Option[CorporationTaxRegistration] = await(service.updateCTRecordWithAckRefs(ackRef, refs))
        result mustBe Some(validHeldCorporationTaxRegistration)
      }
    }
  }

  "setup partial for topup" must {
    val registrationId: String = "testRegId"
    val tID: String = "transID"
    val ackRef: String = "ackRef"
    val companyDetails: CompanyDetails = CompanyDetails(
      "testCompanyName",
      CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
      PPOB("MANUAL", Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"), None, "txid"))),
      "testJurisdiction"
    )
    val sessIds: SessionIds = SessionIds(
      sessionId = "sessID",
      credId = "credID"
    )

    val confRefs: ConfirmationReferences = ConfirmationReferences(
      acknowledgementReference = ackRef,
      transactionId = tID,
      paymentReference = Some("payref"),
      paymentAmount = Some("12")
    )

    val lockedSubmission: CorporationTaxRegistration = CorporationTaxRegistration(
      internalId = "testID",
      registrationID = registrationId,
      formCreationTimestamp = dateTime.toString,
      language = LangConstants.english,
      status = RegistrationStatus.LOCKED,
      companyDetails = Some(companyDetails),
      contactDetails = Some(ContactDetails(
        Some("0123456789"),
        Some("0123456789"),
        Some("test@email.co.uk")
      )),
      tradingDetails = Some(TradingDetails("false")),
      heldTimestamp = Some(Instant.now()),
      confirmationReferences = Some(confRefs),
      sessionIdentifiers = Some(sessIds)
    )

    val businessRegistration: BusinessRegistration = BusinessRegistration(
      registrationId,
      "testTimeStamp",
      "en",
      Some("Director")
    )

    implicit val hc: HeaderCarrier = HeaderCarrier()

    "submit partial if the document is locked" in new Setup {
      when(mockCorpTaxRepo.findOneBySelector(any()))
        .thenReturn(Future.successful(Some(lockedSubmission)))
      when(mockIIConnector.registerInterest(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyBoolean())(any(), any()))
        .thenReturn(Future.successful(true))
      when(mockCorpTaxRepo.retrieveSessionIdentifiers(any()))
        .thenReturn(Future.successful(Some(sessIds)))
      when(mockBRConnector.adminRetrieveMetadata(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(ArgumentMatchers.any()))
        .thenReturn(Future.successful(Some(lockedSubmission)))
      when(mockSubmissionEventService.ctSubmission(any(), any(), any())(any()))
        .thenReturn(Future.successful(HttpResponse(200, "")))
      when(mockCorpTaxRepo.retrieveCompanyDetails(any()))
        .thenReturn(Future.successful(Some(companyDetails)))
      when(mockAuditService.sendEvent(
        ArgumentMatchers.eq("interimCTRegistrationDetails"),
        ArgumentMatchers.any[SubmissionEventDetail](),
        ArgumentMatchers.any(),
      )(ArgumentMatchers.any[HeaderCarrier](), ArgumentMatchers.eq(SubmissionEventDetail.writes)))
        .thenReturn(Future.successful(Success))
      when(mockCorpTaxRepo.updateRegistrationToHeld(eqTo(registrationId), eqTo(confRefs)))
        .thenReturn(Future.successful(Some(lockedSubmission.copy(status = RegistrationStatus.HELD))))
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))

      val result: Boolean = await(service.setupPartialForTopupOnLocked(tID))
      result mustBe true
    }

    "submit partial if the document is locked, even if the audit fails" in new Setup {
      when(mockCorpTaxRepo.findOneBySelector(any()))
        .thenReturn(Future.successful(Some(lockedSubmission)))
      when(mockIIConnector.registerInterest(ArgumentMatchers.anyString(), ArgumentMatchers.anyString(), ArgumentMatchers.anyBoolean())(any(), any()))
        .thenReturn(Future.successful(true))
      when(mockCorpTaxRepo.retrieveSessionIdentifiers(any()))
        .thenReturn(Future.successful(Some(sessIds)))
      when(mockBRConnector.adminRetrieveMetadata(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(ArgumentMatchers.any()))
        .thenReturn(Future.successful(Some(lockedSubmission)))
      when(mockSubmissionEventService.ctSubmission(any(), any(), any())(any()))
        .thenReturn(Future.successful(HttpResponse(200, "")))
      when(mockCorpTaxRepo.retrieveCompanyDetails(any()))
        .thenReturn(Future.successful(Some(companyDetails)))
      when(mockAuditService.sendEvent(
        ArgumentMatchers.eq("interimCTRegistrationDetails"),
        ArgumentMatchers.any[SubmissionEventDetail](),
        ArgumentMatchers.any(),
      )(ArgumentMatchers.any[HeaderCarrier](), ArgumentMatchers.eq(SubmissionEventDetail.writes)))
        .thenReturn(Future.failed(new RuntimeException))
      when(mockCorpTaxRepo.updateRegistrationToHeld(eqTo(registrationId), eqTo(confRefs)))
        .thenReturn(Future.successful(Some(lockedSubmission.copy(status = RegistrationStatus.HELD))))
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))

      val result: Boolean = await(service.setupPartialForTopupOnLocked(tID))
      result mustBe true
    }

    "succeed when trying to submit a partial for a topup if the registration is already submitted" in new Setup {
      when(mockCorpTaxRepo.findOneBySelector(any()))
        .thenReturn(Future.successful(Some(lockedSubmission.copy(
          status = SUBMITTED
        ))))

      await(service.setupPartialForTopupOnLocked(tID)) mustBe true
    }

    "succeed when trying to submit a partial for a topup if the registration is already acknowledged" in new Setup {
      when(mockCorpTaxRepo.findOneBySelector(any()))
        .thenReturn(Future.successful(Some(lockedSubmission.copy(
          status = ACKNOWLEDGED
        ))))

      await(service.setupPartialForTopupOnLocked(tID)) mustBe true
    }

    "abort processing if the document cannot be found at the audit step" in new Setup {

      when(mockCorpTaxRepo.findOneBySelector(any()))
        .thenReturn(Future.successful(Some(lockedSubmission)))
      when(mockCorpTaxRepo.retrieveSessionIdentifiers(any()))
        .thenReturn(Future.successful(Some(sessIds)))
      when(mockBRConnector.adminRetrieveMetadata(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(ArgumentMatchers.any()))
        .thenReturn(Future.successful(Some(lockedSubmission)))
      when(mockSubmissionEventService.ctSubmission(any(), any(), any())(any()))
        .thenReturn(Future.successful(HttpResponse(200, "")))
      when(mockCorpTaxRepo.retrieveCompanyDetails(any()))
        .thenReturn(Future.successful(None))

      intercept[RuntimeException](await(service.setupPartialForTopupOnLocked(tID)))
    }

    "fail to submit a partial for a topup if the session identifiers are not present" in new Setup {

      when(mockCorpTaxRepo.findOneBySelector(any()))
        .thenReturn(Future.successful(Some(lockedSubmission)))
      when(mockCorpTaxRepo.retrieveSessionIdentifiers(any()))
        .thenReturn(Future.successful(None))
      when(mockBRConnector.retrieveMetadataByRegId(ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(BusinessRegistrationSuccessResponse(businessRegistration)))
      when(mockCorpTaxRepo.findOneBySelector(ArgumentMatchers.any()))
        .thenReturn(Future.successful(Some(lockedSubmission)))
      when(mockSubmissionEventService.ctSubmission(any(), any(), any())(any()))
        .thenReturn(Future.successful(HttpResponse(200, "")))
      when(mockCorpTaxRepo.retrieveCompanyDetails(any()))
        .thenReturn(Future.successful(Some(companyDetails)))

      intercept[RuntimeException](await(service.setupPartialForTopupOnLocked(tID)))
    }

    "fail to submit a partial for a topup if the auth prov ID is not present" in new Setup {
      when(mockCorpTaxRepo.findOneBySelector(any()))
        .thenReturn(Future.successful(Some(lockedSubmission.copy(sessionIdentifiers = None))))

      intercept[NoSessionIdentifiersInDocument.type](await(service.setupPartialForTopupOnLocked(tID)))
    }

    "fail to submit a partial for a topup if there is no registration" in new Setup {
      when(mockCorpTaxRepo.findOneBySelector(any()))
        .thenReturn(Future.successful(None))

      intercept[RuntimeException](await(service.setupPartialForTopupOnLocked(tID)))
    }

    "fail to submit a partial for a topup if the registration is not locked" in new Setup {
      when(mockCorpTaxRepo.findOneBySelector(any()))
        .thenReturn(Future.successful(Some(lockedSubmission.copy(
          status = RegistrationStatus.DRAFT
        ))))

      intercept[RuntimeException](await(service.setupPartialForTopupOnLocked(tID)))
    }
  }

  "setup for prepareDocumentForSubmission" must {

    val registrationId: String = "testRegId"
    val tID: String = "transID"
    val ackRef: String = "ackRef"

    val confRefs: ConfirmationReferences = ConfirmationReferences(
      acknowledgementReference = ackRef,
      transactionId = tID,
      paymentReference = Some("payref"),
      paymentAmount = Some("12")
    )

    val lockedSubmission: CorporationTaxRegistration = CorporationTaxRegistration(
      internalId = "testID",
      registrationID = registrationId,
      formCreationTimestamp = "",
      language = LangConstants.english,
      status = RegistrationStatus.LOCKED,
      confirmationReferences = Some(confRefs)
    )

    val noneSubmission: CorporationTaxRegistration = CorporationTaxRegistration(
      internalId = "testID",
      registrationID = registrationId,
      formCreationTimestamp = "",
      language = LangConstants.english,
      confirmationReferences = None

    )

    val refsEmpty: ConfirmationReferences = ConfirmationReferences(
      acknowledgementReference = ackRef,
      transactionId = tID,
      paymentReference = None,
      paymentAmount = None
    )

    val emptySubmission: CorporationTaxRegistration = CorporationTaxRegistration(
      internalId = "testID",
      registrationID = registrationId,
      formCreationTimestamp = "",
      language = LangConstants.english,
      status = RegistrationStatus.LOCKED,
      confirmationReferences = Some(refsEmpty)
    )

    "throw exception when updateConfirmationReferencesAndUpdateStatus is none" in new Setup {
      when(mockSequenceRepo.getNext(any()))
        .thenReturn(Future.successful(1))
      when(mockCorpTaxRepo.updateConfirmationReferencesAndUpdateStatus(any(), any(), any()))
        .thenReturn(Future.successful(None))

      intercept[Exception](await(service prepareDocumentForSubmission(registrationId, "a", confRefs, noneSubmission)))
    }

    "successfully return confirmation refs and not change data when updateConfirmationReferencesAndUpdateStatus has data " in new Setup {
      await(service prepareDocumentForSubmission(registrationId, "a", confRefs, lockedSubmission)) mustBe confRefs
    }

    "successfully update details when confirmationRefsAndPaymentRefsAreEmpty is true " in new Setup {
      when(mockCorpTaxRepo.updateConfirmationReferencesAndUpdateStatus(any(), any(), eqTo(RegistrationStatus.LOCKED)))
        .thenReturn(Future.successful(Some(refsEmpty)))

      await(service prepareDocumentForSubmission(registrationId, "a", refsEmpty, emptySubmission)) mustBe refsEmpty
    }
  }

  "generateAckRef" must {
    "return a new AckRef" in new Setup {
      when(mockSequenceMongoRepository.getNext(any())).thenReturn(Future.successful(1))
      await(service.generateAckRef) mustBe "BRCT00000000001"
    }

    "return an exception when mockSequenceMongoRepository returns an exception" in new Setup {
      when(mockSequenceMongoRepository.getNext(any())).thenReturn(Future.failed(new Exception("failure reason")))
      intercept[Exception](await(service.generateAckRef))
    }
  }
}