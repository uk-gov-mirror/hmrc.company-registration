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

import audit.FailedIncorporationAuditEventDetail
import connectors._
import fixtures.CorporationTaxRegistrationFixture
import models._
import org.mockito.ArgumentMatchers
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.Eventually
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsObject, JsString, Json}
import play.api.test.Helpers._
import repositories.CorporationTaxRegistrationMongoRepository
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, InternalServerException}
import uk.gov.hmrc.play.audit.http.connector.AuditResult.Success
import utils.LogCapturingHelper

import java.time.LocalDate
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class ProcessIncorporationServiceSpec extends PlaySpec with MockitoSugar with CorporationTaxRegistrationFixture with BeforeAndAfterEach with Eventually with LogCapturingHelper {

  val mockIncorporationCheckAPIConnector: IncorporationCheckAPIConnector = mock[IncorporationCheckAPIConnector]
  val mockCTRepository: CorporationTaxRegistrationMongoRepository = mock[CorporationTaxRegistrationMongoRepository]
  val mockAccountService: AccountingDetailsService = mock[AccountingDetailsService]
  val mockSubmissionEventService: SubmissionEventService = mock[SubmissionEventService]
  val mockBRConnector: BusinessRegistrationConnector = mock[BusinessRegistrationConnector]
  val mockAuthConnector: AuthConnector = mock[AuthConnector]
  val mockAuditService: AuditService = mock[AuditService]
  val mockSendEmailService: SendEmailService = mock[SendEmailService]

  override def beforeEach(): Unit = {
    resetMocks()
  }

  def resetMocks(): Unit = reset(
    mockAuthConnector,
    mockAuditService,
    mockIncorporationCheckAPIConnector,
    mockCTRepository,
    mockSubmissionEventService,
    mockBRConnector,
    mockSendEmailService,
    mockAccountService
  )


  trait mockService extends ProcessIncorporationService {
    val incorporationCheckAPIConnector: IncorporationCheckAPIConnector = mockIncorporationCheckAPIConnector
    val ctRepository: CorporationTaxRegistrationMongoRepository = mockCTRepository
    val accountingService: AccountingDetailsService = mockAccountService
    val submissionEventService: SubmissionEventService = mockSubmissionEventService
    val brConnector: BusinessRegistrationConnector = mockBRConnector
    val auditService: AuditService = mockAuditService
    val microserviceAuthConnector: AuthConnector = mockAuthConnector
    val sendEmailService: SendEmailService = mockSendEmailService
    override val addressLine4FixRegID: String = "false"
    override val amendedAddressLine4: String = ""
    override val blockageLoggingDay: String = "MON,TUE,WED,THU,FRI"
    override val blockageLoggingTime: String = "08:00:00_17:00:00"

    override def inWorkingHours: Boolean = true
  }

  trait Setup {
    object Service extends mockService {
      implicit val ec: ExecutionContext = global
    }
  }

  class SetupWithAddressLine4Fix(regId: String, addressLine4: String) {
    object Service extends mockService {
      override val addressLine4FixRegID: String = regId
      override val amendedAddressLine4: String = addressLine4
      implicit val ec: ExecutionContext = global
    }
  }

  def date(yyyyMMdd: String): LocalDate = LocalDate.parse(yyyyMMdd)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val timepoint = "123456789"
  val testAckRef: String = UUID.randomUUID.toString
  val testRegId: String = UUID.randomUUID.toString
  val transId: String = UUID.randomUUID().toString
  val validCR: CorporationTaxRegistration = validHeldCTRegWithData(ackRef = Some(testAckRef))
    .copy(
      accountsPreparation = Some(AccountPrepDetails(AccountPrepDetails.COMPANY_DEFINED, Some(date("2017-01-01")))),
      verifiedEmail = Some(Email("testemail.com", "", linkSent = true, verified = true, returnLinkEmailSent = true)
      )
  )

  import models.RegistrationStatus._

  val submittedCR: CorporationTaxRegistration = validCR.copy(status = SUBMITTED)
  val acknowledgedCR: CorporationTaxRegistration = validCR.copy(status = ACKNOWLEDGED)
  val failCaseCR: CorporationTaxRegistration = validCR.copy(status = DRAFT)
  val incorpSuccess: IncorpUpdate = IncorpUpdate(transId, "accepted", Some("012345"), Some(LocalDate.of(2016, 8, 10)), timepoint)
  val incorpRejected: IncorpUpdate = IncorpUpdate(transId, "rejected", None, None, timepoint, Some("testReason"))
  val submissionCheckResponseSingle: SubmissionCheckResponse = SubmissionCheckResponse(Seq(incorpSuccess), "testNextLink")
  val submissionCheckResponseDouble: SubmissionCheckResponse = SubmissionCheckResponse(Seq(incorpSuccess, incorpSuccess), "testNextLink")
  val submissionCheckResponseNone: SubmissionCheckResponse = SubmissionCheckResponse(Seq(), "testNextLink")

  def sub(a: String, others: Option[(String, String, String, String)] = None): String = {
    val extra = others match {
      case None => ""
      case Some((crn, active, firstPrep, intended)) =>
        s"""
           |  ,
           |  "crn" : "$crn",
           |  "companyActiveDate": "$active",
           |  "startDateOfFirstAccountingPeriod": "$firstPrep",
           |  "intendedAccountsPreparationDate": "$intended"
           |""".stripMargin
    }

    s"""{  "acknowledgementReference" : "$a",
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
       |  "companyOfficeNumber" : "001",
       |  "hasCompanyTakenOverBusiness" : false,
       |  "companyMemberOfGroup" : false,
       |  "companiesHouseCompanyName" : "testCompaniesHouseName",
       |  "returnsOnCT61" : false,
       |  "companyACharity" : false,
       |  "businessAddress" : {"line1" : "1 Test Avenue", "line2" : "Oakengates", "line3" : "Telford",
       |  "line4" : "Shropshire", "postcode" : "ZZ1 1ZZ", "country" : "United Kingdom"},
       |  "businessContactDetails" : {"phoneNumber" : "0123457889","mobileNumber" : "07654321000","email" : "test@email.com"}
       |  $extra
       |  }
       |  }
       |}""".stripMargin
  }

  def topUpSub(s: String, a: String, crn: String, active: String, firstPrep: String, intended: String): String =
    s"""{
       |  "status" : "$s",
       |  "acknowledgementReference" : "$a",
       |  "corporationTax" : {
       |  "crn" : "$crn",
       |  "companyActiveDate": "$active",
       |  "startDateOfFirstAccountingPeriod": "$firstPrep",
       |  "intendedAccountsPreparationDate": "$intended"
       |  }
       |  }
       |""".stripMargin


  def topUpRejSub(s: String, a: String): String =
    s"""{
       |  "status" : "$s",
       |  "acknowledgementReference" : "$a"
       |  }
       |""".stripMargin


  val crn = "012345"
  val exampleDate = "2012-12-12"
  val exampleDate1 = "2020-05-10"
  val exampleDate2 = "2025-06-06"
  val dates: SubmissionDates = SubmissionDates(date(exampleDate), date(exampleDate1), date(exampleDate2))
  val acceptedStatus = "Accepted"
  val rejectedStatus = "Rejected"
  val interimSubmission: JsObject = Json.parse(sub(testAckRef)).as[JsObject]
  val validDesSubmission: JsObject = Json.parse(sub(testAckRef, Some((crn, exampleDate, exampleDate1, exampleDate2)))).as[JsObject]
  val validTopUpDesSubmission: JsObject = Json.parse(topUpSub(acceptedStatus, testAckRef, crn, exampleDate, exampleDate1, exampleDate2)).as[JsObject]
  val validRejectedTopUpDesSubmission: JsObject = Json.parse(topUpRejSub(rejectedStatus, testAckRef)).as[JsObject]

  "formatDate" must {
    "format a LocalDate into the format yyyy-mm-dd" in new Setup {
      Service.formatDate(LocalDate.of(1970,1,1)) mustBe "1970-01-01"
    }
  }

  "deleteRejectedSubmissionData" must {
    "return true if both br and ct data have been removed" in new Setup {
      when(mockCTRepository.removeTaxRegistrationById(ArgumentMatchers.eq(testRegId)))
        .thenReturn(Future.successful(true))
      when(mockBRConnector.removeMetadata(ArgumentMatchers.eq(testRegId))(ArgumentMatchers.any()))
        .thenReturn(Future.successful(true))

      await(Service.deleteRejectedSubmissionData(testRegId)) mustBe true
    }

    "return an exception if ct data has not been fully removed" in new Setup {
      when(mockCTRepository.removeTaxRegistrationById(ArgumentMatchers.eq(testRegId)))
        .thenReturn(Future.successful(false))
      when(mockBRConnector.removeMetadata(ArgumentMatchers.eq(testRegId))(ArgumentMatchers.any()))
        .thenReturn(Future.successful(true))

      intercept[FailedToDeleteSubmissionData.type](await(Service.deleteRejectedSubmissionData(testRegId)))
    }

    "return an exception if br data has not been fully removed" in new Setup {
      when(mockCTRepository.removeTaxRegistrationById(ArgumentMatchers.eq(testRegId)))
        .thenReturn(Future.successful(true))
      when(mockBRConnector.removeMetadata(ArgumentMatchers.eq(testRegId))(ArgumentMatchers.any()))
        .thenReturn(Future.successful(false))

      intercept[FailedToDeleteSubmissionData.type](await(Service.deleteRejectedSubmissionData(testRegId)))
    }

    "return an exception if an exception occurs while removing ct data" in new Setup {
      when(mockCTRepository.removeTaxRegistrationById(ArgumentMatchers.eq(testRegId)))
        .thenReturn(Future.failed(new Exception))
      when(mockBRConnector.removeMetadata(ArgumentMatchers.eq(testRegId))(ArgumentMatchers.any()))
        .thenReturn(Future.successful(false))

      intercept[Exception](await(Service.deleteRejectedSubmissionData(testRegId)))
    }

    "return an exception if an exception occurs while removing br data" in new Setup {
      when(mockCTRepository.removeTaxRegistrationById(ArgumentMatchers.eq(testRegId)))
        .thenReturn(Future.successful(true))
      when(mockBRConnector.removeMetadata(ArgumentMatchers.eq(testRegId))(ArgumentMatchers.any()))
        .thenReturn(Future.failed(new Exception))

      intercept[Exception](await(Service.deleteRejectedSubmissionData(testRegId)))
    }
  }


  "activeDates" must {
    "return DoNotIntendToTrade if that was selected" in new Setup {

      import AccountingDetails.NOT_PLANNING_TO_YET

      Service.activeDate(AccountingDetails(NOT_PLANNING_TO_YET, None), incorpSuccess.incorpDate.get) mustBe DoNotIntendToTrade
    }
    "return ActiveOnIncorporation if that was selected" in new Setup {

      import AccountingDetails.WHEN_REGISTERED

      Service.activeDate(AccountingDetails(WHEN_REGISTERED, None), incorpSuccess.incorpDate.get) mustBe ActiveOnIncorporation
    }
    "return ActiveOnIncorporation if the active date is before the incorporation date" in new Setup {

      import AccountingDetails.FUTURE_DATE

      val tradeDate = "2016-08-09"
      Service.activeDate(AccountingDetails(FUTURE_DATE, Some(tradeDate)), incorpSuccess.incorpDate.get) mustBe ActiveOnIncorporation
    }
    "return ActiveInFuture with a date if that was selected" in new Setup {

      import AccountingDetails.FUTURE_DATE

      val tradeDate = "2017-01-01"
      Service.activeDate(AccountingDetails(FUTURE_DATE, Some(tradeDate)), incorpSuccess.incorpDate.get) mustBe ActiveInFuture(date(tradeDate))
    }
  }

  "calculateDates" must {

    "return valid dates if correct detail is passed" in new Setup {
      when(mockAccountService.calculateSubmissionDates(ArgumentMatchers.any(), ArgumentMatchers.any(), ArgumentMatchers.any()))
        .thenReturn(dates)

      val result: Future[SubmissionDates] = Service.calculateDates(incorpSuccess, validCR.accountingDetails, validCR.accountsPreparation)

      await(result) mustBe dates
    }
  }

  "updateSubmission" must {
    trait SetupNoProcess {
      object Service extends mockService {mockAccountService
        implicit val ec: ExecutionContext = global

        override def updateHeldSubmission(item: IncorpUpdate, ctReg: CorporationTaxRegistration, journeyId: String, isAdmin: Boolean = false)(implicit hc: HeaderCarrier): Future[Boolean] = Future.successful(true)
      }
    }
    "return true for a DES/HIP ready submission" in new SetupNoProcess {
      when(mockCTRepository.findOneBySelector(mockCTRepository.transIdSelector(ArgumentMatchers.eq(transId))))
        .thenReturn(Future.successful(Some(validCR)))

      await(Service.updateSubmissionWithIncorporation(incorpSuccess, validCR)) mustBe true
    }
    "return false for a Locked registration" in new SetupNoProcess {
      val lockedReg: CorporationTaxRegistration = validCR.copy(status = LOCKED)
      when(mockCTRepository.findOneBySelector(mockCTRepository.transIdSelector(ArgumentMatchers.eq(transId))))
        .thenReturn(Future.successful(Some(lockedReg)))

      await(Service.updateSubmissionWithIncorporation(incorpSuccess, lockedReg)) mustBe false
    }
    "return true for a submission that is already Submitted" in new SetupNoProcess {
      when(mockCTRepository.findOneBySelector(mockCTRepository.transIdSelector(ArgumentMatchers.eq(transId))))
        .thenReturn(Future.successful(Some(submittedCR)))

      await(Service.updateSubmissionWithIncorporation(incorpSuccess, submittedCR)) mustBe true
    }
    "return true for a submission that is already Acknowledged" in new SetupNoProcess {
      when(mockCTRepository.findOneBySelector(mockCTRepository.transIdSelector(ArgumentMatchers.eq(transId))))
        .thenReturn(Future.successful(Some(acknowledgedCR)))

      await(Service.updateSubmissionWithIncorporation(incorpSuccess, submittedCR)) mustBe true
    }
    "throw UnexpectedStatus for a submission that is neither 'Held' nor 'Submitted'" in new SetupNoProcess {
      when(mockCTRepository.findOneBySelector(mockCTRepository.transIdSelector(ArgumentMatchers.eq(transId))))
        .thenReturn(Future.successful(Some(failCaseCR)))

      intercept[Exception] {
        await(Service.updateSubmissionWithIncorporation(incorpSuccess, failCaseCR))
      }
    }

  }

  "processIncorporationUpdate" must {

    class SetupBoolean(boole: Boolean) {
      object Service extends mockService {
        implicit val ec: ExecutionContext = global
        override def updateSubmissionWithIncorporation(item: IncorpUpdate, ctReg: CorporationTaxRegistration, isAdmin: Boolean = false)(
          implicit hc: HeaderCarrier): Future[Boolean] = Future.successful(boole)
      }
    }

    "return a future true when processing an accepted incorporation" in new SetupBoolean(true) {
      when(mockCTRepository.findOneBySelector(mockCTRepository.transIdSelector(ArgumentMatchers.eq(transId))))
        .thenReturn(Future.successful(Some(validCR)))
      when(mockSendEmailService.sendVATEmail(ArgumentMatchers.eq("testemail.com"), ArgumentMatchers.any())(ArgumentMatchers.any[HeaderCarrier]())).thenReturn(Future.successful(true))
      await(Service.processIncorporationUpdate(incorpSuccess)) mustBe true
    }

    "return a future true when processing an accepted incorporation and the email fails to send" in new SetupBoolean(true) {
      when(mockCTRepository.findOneBySelector(mockCTRepository.transIdSelector(ArgumentMatchers.eq(transId))))
        .thenReturn(Future.successful(Some(validCR)))
      when(mockSendEmailService.sendVATEmail(ArgumentMatchers.eq("testemail.com"), ArgumentMatchers.any())(ArgumentMatchers.any[HeaderCarrier]()))
        .thenReturn(Future.failed(new EmailErrorResponse(503)))

      await(Service.processIncorporationUpdate(incorpSuccess)) mustBe true
    }

    "return a future false when processing an accepted incorporation returns a false" in new SetupBoolean(false) {
      when(mockCTRepository.findOneBySelector(mockCTRepository.transIdSelector(ArgumentMatchers.eq(transId))))
        .thenReturn(Future.successful(Some(validCR)))
      await(Service.processIncorporationUpdate(incorpSuccess)) mustBe false
    }

    "return a future true when processing a rejected incorporation" in new Setup {
      when(mockCTRepository.findOneBySelector(mockCTRepository.transIdSelector(ArgumentMatchers.eq(transId))))
        .thenReturn(Future.successful(Some(validCR)))

      when(mockCTRepository.updateSubmissionStatus(ArgumentMatchers.any(), ArgumentMatchers.any())).thenReturn(Future.successful("rejected"))

      when(mockAuditService.sendEvent(
        ArgumentMatchers.eq("failedIncorpInformation"),
        ArgumentMatchers.eq(FailedIncorporationAuditEventDetail(
          validCR.registrationID,
          "testReason"
        )),
        ArgumentMatchers.any()
      )(ArgumentMatchers.any[HeaderCarrier](), ArgumentMatchers.eq(FailedIncorporationAuditEventDetail.format)))
        .thenReturn(Future.successful(Success))

      when(mockSubmissionEventService.topUpCTSubmission(ArgumentMatchers.any(), ArgumentMatchers.any(), ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.successful(HttpResponse(200, "")))

      when(mockCTRepository.removeTaxRegistrationById(ArgumentMatchers.eq(validCR.registrationID)))
        .thenReturn(Future.successful(true))

      when(mockBRConnector.removeMetadata(ArgumentMatchers.eq(validCR.registrationID))(ArgumentMatchers.any()))
        .thenReturn(Future.successful(true))

      when(mockCTRepository.removeUnnecessaryRegistrationInformation(ArgumentMatchers.eq(validCR.registrationID)))
        .thenReturn(Future.successful(true))

      await(Service.processIncorporationUpdate(incorpRejected)) mustBe true
    }

    "return a future false, do not top up on rejected incorporation in LOCKED state" in new Setup {
      when(mockCTRepository.findOneBySelector(mockCTRepository.transIdSelector(ArgumentMatchers.eq(transId))))
        .thenReturn(Future.successful(Some(validCR.copy(status = LOCKED))))

      when(mockAuditService.sendEvent(
        ArgumentMatchers.eq("failedIncorpInformation"),
        ArgumentMatchers.eq(FailedIncorporationAuditEventDetail(
          validCR.registrationID,
          "testReason"
        )),
        ArgumentMatchers.any()
      )(ArgumentMatchers.any[HeaderCarrier](), ArgumentMatchers.eq(FailedIncorporationAuditEventDetail.format)))
        .thenReturn(Future.successful(Success))

      await(Service.processIncorporationUpdate(incorpRejected)) mustBe false
    }

    "return an exception when processing a rejected incorporation and Des returns a 500" in new Setup {
      when(mockCTRepository.findOneBySelector(mockCTRepository.transIdSelector(ArgumentMatchers.eq(transId))))
        .thenReturn(Future.successful(Some(validCR)))
      when(mockCTRepository.updateSubmissionStatus(ArgumentMatchers.any(), ArgumentMatchers.any())).thenReturn(Future.successful("rejected"))
      when(mockAuditService.sendEvent(
        ArgumentMatchers.eq("failedIncorpInformation"),
        ArgumentMatchers.eq(FailedIncorporationAuditEventDetail(
          validCR.registrationID,
          "testReason"
        )),
        ArgumentMatchers.any()
      )(ArgumentMatchers.any[HeaderCarrier](), ArgumentMatchers.eq(FailedIncorporationAuditEventDetail.format)))
        .thenReturn(Future.successful(Success))
      when(mockSubmissionEventService.topUpCTSubmission(ArgumentMatchers.any(), ArgumentMatchers.any(), ArgumentMatchers.any())(ArgumentMatchers.any()))
        .thenReturn(Future.failed(new InternalServerException("DES/HIP returned a 500")))
      when(mockCTRepository.removeTaxRegistrationById(ArgumentMatchers.eq(validCR.registrationID)))
        .thenReturn(Future.successful(true))
      when(mockBRConnector.removeMetadata(ArgumentMatchers.eq(validCR.registrationID))(ArgumentMatchers.any()))
        .thenReturn(Future.successful(true))

      intercept[InternalServerException] {
        await(Service.processIncorporationUpdate(incorpRejected))
      }
    }

    "log a pagerduty if no reg document is found" in new Setup {

      when(mockCTRepository.findOneBySelector(mockCTRepository.transIdSelector(ArgumentMatchers.eq(transId))))
        .thenReturn(Future.successful(None))

      when(mockCTRepository.updateSubmissionStatus(ArgumentMatchers.any(), ArgumentMatchers.any())).thenReturn(Future.successful("rejected"))

      when(mockCTRepository.removeTaxRegistrationById(ArgumentMatchers.eq(validCR.registrationID)))
        .thenReturn(Future.successful(true))

      when(mockBRConnector.removeMetadata(ArgumentMatchers.eq(validCR.registrationID))(ArgumentMatchers.any()))
        .thenReturn(Future.successful(true))

      withCaptureOfLoggingFrom(Service.logger) { logEvents =>
        await(Service.processIncorporationUpdate(incorpSuccess)) mustBe true

        logEvents.size mustBe 2
        val res = logEvents.map(_.getMessage) contains "[Service] CT_ACCEPTED_NO_REG_DOC_II_SUBS_DELETED"

        res mustBe true
      }
    }
  }

  "addressLine4Fix" must {

    val regId = "reg-12345"
    val addressLine4 = "testAL4"
    val encodedAddressLine4 = "dGVzdEFMNA=="
    val addressLine4Json = JsString(addressLine4)

    "amend a held submissions' address line 4 with the one provided through config if the reg id's match" in new SetupWithAddressLine4Fix(regId, encodedAddressLine4) {
      val result: JsObject = Service.addressLine4Fix(regId, heldJson)
      (result \ "registration" \ "corporationTax" \ "businessAddress" \ "line4").toOption mustBe Some(addressLine4Json)
    }

    "do not amend a held submissions' address line 4 if the reg id's do not match" in new SetupWithAddressLine4Fix("otherRegID", encodedAddressLine4) {
      val result: JsObject = Service.addressLine4Fix(regId, heldJson)
      result mustBe heldJson
    }
  }
}
