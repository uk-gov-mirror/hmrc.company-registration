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

package services.admin

import config.LangConstants
import connectors.{BusinessRegistrationConnector, IncorporationInformationConnector}
import models._
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.Eventually
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.mvc.{AnyContent, Request}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.CorporationTaxRegistrationMongoRepository
import services.{AuditService, FailedToDeleteSubmissionData, SubmissionEventService}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, NotFoundException, UpstreamErrorResponse}
import uk.gov.hmrc.mongo.lock.LockService
import uk.gov.hmrc.play.audit.http.connector.AuditResult

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class AdminServiceSpec extends PlaySpec with MockitoSugar with BeforeAndAfterEach with Eventually {

  val mockAuditService: AuditService = mock[AuditService]
  val mockIncorpInfoConnector: IncorporationInformationConnector = mock[IncorporationInformationConnector]
  val mockCorpTaxRegistrationRepo: CorporationTaxRegistrationMongoRepository = mock[CorporationTaxRegistrationMongoRepository]
  val mockBusRegConnector: BusinessRegistrationConnector = mock[BusinessRegistrationConnector]
  val mockSubmissionEventService: SubmissionEventService = mock[SubmissionEventService]
  val mockLockService: LockService = mock[LockService]

  class Setup {
    val service: AdminService = new AdminService {
      val auditService: AuditService = mockAuditService
      val incorpInfoConnector: IncorporationInformationConnector = mockIncorpInfoConnector
      val corpTaxRegRepo: CorporationTaxRegistrationMongoRepository = mockCorpTaxRegistrationRepo
      val brConnector: BusinessRegistrationConnector = mockBusRegConnector
      val submissionEventService: SubmissionEventService = mockSubmissionEventService
      override val lockKeeper: LockService = mockLockService
      implicit val ec: ExecutionContext = global
      override val staleAmount: Int = 10
      override val clearAfterXDays: Int = 90
      override val ignoredDocs: Set[String] = Set("1", "2", "3", "4", "5")
    }
    val docInfo: service.DocumentInfo = service.DocumentInfo(regId, "draft", Instant.now)
  }

  override def beforeEach(): Unit = {
    reset(mockAuditService)
    reset(mockBusRegConnector)
    reset(mockCorpTaxRegistrationRepo)
    reset(mockSubmissionEventService)
    reset(mockIncorpInfoConnector)
    reset(mockLockService)
  }

  val regId = "reg-id-12345"
  val ackRef = "ack-ref-12345"
  val transId = "trans-12345"

  implicit val hc: HeaderCarrier = HeaderCarrier()
  implicit val req: Request[AnyContent] = FakeRequest()

  val sessionIdData: SessionIdData = SessionIdData(Some("session-id"), Some("credId"), Some("fakeCompanyName"), Some(ackRef))

  def makeSessionReg(sessionIds: Option[SessionIdData]): CorporationTaxRegistration = CorporationTaxRegistration(
    internalId = "testID",
    registrationID = "registrationId",
    formCreationTimestamp = "dd-mm-yyyy",
    language = LangConstants.english,
    status = RegistrationStatus.HELD,
    companyDetails = Some(CompanyDetails(
      sessionIds.flatMap(ids => sessionIds.flatMap(_.companyName)).getOrElse("companyName"),
      CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
      PPOB("MANUAL", Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"), None, "txid"))),
      "testJurisdiction"
    )),
    contactDetails = Some(ContactDetails(
      Some("0123456789"),
      Some("0123456789"),
      Some("test@email.co.uk")
    )),
    tradingDetails = Some(TradingDetails("false")),
    heldTimestamp = None,
    confirmationReferences = Some(ConfirmationReferences(
      acknowledgementReference = sessionIds.flatMap(ids => sessionIds.flatMap(_.ackRef)).getOrElse("ackRef"),
      transactionId = "tID",
      paymentReference = Some("payref"),
      paymentAmount = Some("12")
    )),
    sessionIdentifiers = sessionIds.map(ids => SessionIds(ids.sessionId.getOrElse(""), ids.credId.getOrElse("")))
  )

  "fetchSessionIdData" must {
    "fetch session id data if it exists" in new Setup {
      val data: Some[SessionIdData] = Some(sessionIdData)
      val reg: CorporationTaxRegistration = makeSessionReg(data)

      when(mockCorpTaxRegistrationRepo.findOneBySelector(any()))
        .thenReturn(Future.successful(Some(reg)))

      await(service.fetchSessionIdData(regId)) mustBe data
    }
  }

  "adminCTUTRCheck" must {
    "return the Status and presence of CTUTR as valid JSON" when {
      "using a valid AckRef" in new Setup {
        val id = "BRCT09876543210"
        val expected: JsValue = Json.parse(
          """
            |{
            | "status": "04",
            | "ctutr": true
            |}
           """.stripMargin
        )

        when(mockCorpTaxRegistrationRepo.retrieveStatusAndExistenceOfCTUTR(any()))
          .thenReturn(Future.successful(Option("04" -> true)))

        val result: JsObject = await(service.ctutrCheck(id))

        result mustBe expected
      }
      "the valid AckRef retrieves no stored document" in new Setup {
        val id = "BRCT09876543210"
        when(mockCorpTaxRegistrationRepo.retrieveStatusAndExistenceOfCTUTR(any()))
          .thenReturn(Future.successful(None))

        val result: JsObject = await(service.ctutrCheck(id))
        result mustBe Json.parse("{}")
      }
    }
  }

  "updateRegistrationWithAdminCTReference" must {
    val timestamp = "timestamp"

    def makeReg(utr: Option[String]) = CorporationTaxRegistration(
      internalId = "testID",
      registrationID = "registrationId",
      formCreationTimestamp = "dd-mm-yyyy",
      language = LangConstants.english,
      status = RegistrationStatus.HELD,
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
      tradingDetails = Some(TradingDetails("false")),
      heldTimestamp = None,
      confirmationReferences = Some(ConfirmationReferences(
        acknowledgementReference = "ackRef",
        transactionId = "tID",
        paymentReference = Some("payref"),
        paymentAmount = Some("12")
      )),
      acknowledgementReferences = Some(AcknowledgementReferences(
        utr, timestamp, utr.map(_ => "04").getOrElse("06")
      ))
    )
  }

  "deleteRejectedSubmissionData" must {
    "delete a registration" when {
      "it exists" in new Setup {
        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.successful(true))
        when(mockIncorpInfoConnector.cancelSubscription(any(), any(), any())(any()))
          .thenReturn(Future.successful(true))
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.successful(true))

        await(service.adminDeleteSubmission(docInfo, Some(transId))) mustBe true
      }
    }
    "not delete a registration" when {
      "it does not exist any of the databases" in new Setup {
        val inputs: List[(Boolean, Boolean)] = List((true, false), (false, true), (false, false))

        for ((brResult, crResult) <- inputs) {
          when(mockBusRegConnector.adminRemoveMetadata(any()))
            .thenReturn(
              Future.successful(brResult)
            )
          when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
            .thenReturn(
              Future.successful(crResult)
            )
          when(mockIncorpInfoConnector.cancelSubscription(any(), any(), any())(any()))
            .thenReturn(
              Future.successful(true)
            )

          intercept[FailedToDeleteSubmissionData.type](await(service.adminDeleteSubmission(docInfo, Some(transId))))
        }
      }
      "an error is thrown" in new Setup {
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.failed(new RuntimeException))

        intercept[RuntimeException](await(service.adminDeleteSubmission(docInfo, Some(transId))))
      }
    }
  }

  "updateTransactionId" must {

    val updateFrom = "TRANS-123456789"
    val updateTo = "TRANS-0123456789"

    "update the transaction id" when {
      "the transaction id exists in the database" in new Setup {
        when(mockCorpTaxRegistrationRepo.updateTransactionId(any(), any()))
          .thenReturn(Future.successful(updateTo))

        await(service.updateTransactionId(updateFrom, updateTo)) mustBe true
      }
    }

    "not update a transaction id" when {
      "the transaction id does not exist in the database" in new Setup {
        when(mockCorpTaxRegistrationRepo.updateTransactionId(any(), any()))
          .thenReturn(Future.failed(new RuntimeException("")))

        await(service.updateTransactionId(updateFrom, updateTo)) mustBe false
      }
    }
  }

  "deleteLimboCase" must {

    val regId = "myRegID"
    val companyName = "dtl ynapmoc"

    def makeReg(status: String, compName: String, ackRefExists: Boolean, companyDetailsExists: Boolean = true) = CorporationTaxRegistration(
      internalId = "testID",
      registrationID = regId,
      formCreationTimestamp = "dd-mm-yyyy",
      language = LangConstants.english,
      status = status,
      companyDetails = if (companyDetailsExists) {
        Some(CompanyDetails(
          compName,
          CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
          PPOB("MANUAL", Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"), None, "txid"))),
          "testJurisdiction"
        ))
      } else {
        None
      },
      contactDetails = Some(ContactDetails(
        Some("0123456789"),
        Some("0123456789"),
        Some("test@email.co.uk")
      )),
      tradingDetails = Some(TradingDetails("false")),
      heldTimestamp = None,
      confirmationReferences = if (ackRefExists) {
        Some(ConfirmationReferences(
          acknowledgementReference = "ackRef",
          transactionId = "tID",
          paymentReference = Some("payref"),
          paymentAmount = Some("12")
        ))
      } else {
        None
      },
      acknowledgementReferences = None
    )
  }

  "deleteStaleDocuments" must {
    val exampleDoc = CorporationTaxRegistration("id", "regid", "draft", "timestamp", "lang")
    "return a count of documents retrieved and deleted" when {
      "only one is older than 90 days" in new Setup {
        when(mockCorpTaxRegistrationRepo.retrieveStaleDocuments(any(), any()))
          .thenReturn(Future.successful(List(exampleDoc)))
        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.successful(true))
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.successful(true))
        await(service.deleteStaleDocuments()) mustBe 1
      }
      "three are older than 90 days" in new Setup {
        when(mockCorpTaxRegistrationRepo.retrieveStaleDocuments(any(), any()))
          .thenReturn(Future.successful(List.fill(3)(exampleDoc)))
        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.successful(true))
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.successful(true))
        await(service.deleteStaleDocuments()) mustBe 3
      }
      "three are older than 90 days, and one fails to delete BR data" in new Setup {
        when(mockCorpTaxRegistrationRepo.retrieveStaleDocuments(any(), any()))
          .thenReturn(Future.successful(List.fill(3)(exampleDoc)))
        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.successful(true), Future.successful(false), Future.successful(true))
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.successful(true), Future.successful(true), Future.successful(true))
        await(service.deleteStaleDocuments()) mustBe 2
      }
      "three are older than 90 days, and one has a status of submitted" in new Setup {
        when(mockCorpTaxRegistrationRepo.retrieveStaleDocuments(any(), any()))
          .thenReturn(Future.successful(List.fill(2)(exampleDoc).::(exampleDoc.copy(status = "submitted"))))
        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.successful(true))
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.successful(true))
        await(service.deleteStaleDocuments()) mustBe 2
      }
      "three are older than 90 days, and one is on the allow list" in new Setup {
        when(mockCorpTaxRegistrationRepo.retrieveStaleDocuments(any(), any()))
          .thenReturn(Future.successful(List.fill(2)(exampleDoc).::(exampleDoc.copy(registrationID = "2"))))
        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.successful(true))
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.successful(true))
        await(service.deleteStaleDocuments()) mustBe 2
      }
    }
  }

  "processStaleDocument" must {
    val exampleDoc = CorporationTaxRegistration("id", "regid", "draft", "timestamp", "lang")
    "return true" when {
      "the document is draft, has no confirmation references and successfully deletes BR and CR document" in new Setup {

        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.successful(true))
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.successful(true))

        await(service.processStaleDocument(exampleDoc)) mustBe true
      }

      "the document is draft, has confirmation references and successfully deletes BR and CR document" in new Setup {
        val confRefExampleDoc: CorporationTaxRegistration = exampleDoc.copy(confirmationReferences = Some(ConfirmationReferences("", "", None, None)))

        when(mockIncorpInfoConnector.checkCompanyIncorporated(any())(any()))
          .thenReturn(Future.successful(None))
        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.successful(true))
        when(mockIncorpInfoConnector.cancelSubscription(any(), any(), any())(any()))
          .thenReturn(Future.successful(true))
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.successful(true))

        await(service.processStaleDocument(confRefExampleDoc)) mustBe true

        verify(mockIncorpInfoConnector, times(1)).cancelSubscription(any(), any(), any())(any())
      }

      "the document is draft, has confirmation references and successfully deletes BR and CR document but the subscription has an old regime" in new Setup {
        val confRefExampleDoc: CorporationTaxRegistration = exampleDoc.copy(confirmationReferences = Some(ConfirmationReferences("", "", None, None)))

        when(mockIncorpInfoConnector.checkCompanyIncorporated(any())(any()))
          .thenReturn(Future.successful(None))
        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.successful(true))
        when(mockIncorpInfoConnector.cancelSubscription(any(), any(), any())(any()))
          .thenReturn(
            Future.failed(new NotFoundException("failed to cancel sub")),
            Future.successful(true)
          )
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.successful(true))

        await(service.processStaleDocument(confRefExampleDoc)) mustBe true

        verify(mockIncorpInfoConnector, times(1)).cancelSubscription(any(), any(), ArgumentMatchers.eq(false))(any())
        verify(mockIncorpInfoConnector, times(1)).cancelSubscription(any(), any(), ArgumentMatchers.eq(true))(any())
      }

      "the document is draft, has confirmation references and successfully deletes BR and CR document, but no subs" in new Setup {
        val confRefExampleDoc: CorporationTaxRegistration = exampleDoc.copy(confirmationReferences = Some(ConfirmationReferences("", "", None, None)))

        when(mockIncorpInfoConnector.checkCompanyIncorporated(any())(any()))
          .thenReturn(Future.successful(None))
        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.successful(true))
        when(mockIncorpInfoConnector.cancelSubscription(any(), any(), any())(any()))
          .thenReturn(
            Future.failed(new NotFoundException("failed to cancel sub")),
            Future.failed(new NotFoundException("failed to cancel another sub"))
          )
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.successful(true))

        await(service.processStaleDocument(confRefExampleDoc)) mustBe true

        verify(mockIncorpInfoConnector, times(1)).cancelSubscription(any(), any(), ArgumentMatchers.eq(false))(any())
        verify(mockIncorpInfoConnector, times(1)).cancelSubscription(any(), any(), ArgumentMatchers.eq(true))(any())
      }

      "the document is held or locked, has confirmation references and successfully deletes BR and CR document" in new Setup {
        val confRefExampleDoc: CorporationTaxRegistration = exampleDoc.copy(status = "held", confirmationReferences = Some(ConfirmationReferences("", "", None, None)))

        when(mockIncorpInfoConnector.checkCompanyIncorporated(any())(any()))
          .thenReturn(Future.successful(None))
        when(mockSubmissionEventService.topUpCTSubmission(any(), any(), any())(any()))
          .thenReturn(Future.successful(HttpResponse(200, "")))
        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.successful(true))
        when(mockIncorpInfoConnector.cancelSubscription(any(), any(), any())(any()))
          .thenReturn(Future.successful(true))
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.successful(true))
        when(mockAuditService.sendEvent(any(), any(), any())(any(), any()))
          .thenReturn(Future.successful(AuditResult.Success))

        await(service.processStaleDocument(confRefExampleDoc)) mustBe true

        verify(mockIncorpInfoConnector, times(1)).cancelSubscription(any(), any(), any())(any())
      }
    }

    "return false" when {
      "the document is held or locked with a CRN" in new Setup {
        val confRefWithCRNExampleDoc: CorporationTaxRegistration = exampleDoc.copy(status = "held", confirmationReferences = Some(ConfirmationReferences("", "transID", None, None)))

        when(mockIncorpInfoConnector.checkCompanyIncorporated(any())(any()))
          .thenReturn(Future.successful(Some("CRN found")))

        await(service.processStaleDocument(confRefWithCRNExampleDoc)) mustBe false
      }

      "the document is held or locked with failure to send a rejection topup" in new Setup {
        val confRefWithCRNExampleDoc: CorporationTaxRegistration = exampleDoc.copy(status = "locked", confirmationReferences = Some(ConfirmationReferences("", "transID", None, None)))

        when(mockIncorpInfoConnector.checkCompanyIncorporated(any())(any()))
          .thenReturn(Future.successful(None))
        when(mockSubmissionEventService.topUpCTSubmission(any(), any(), any())(any()))
          .thenReturn(Future.failed(UpstreamErrorResponse("test", 400, 400, Map())))

        await(service.processStaleDocument(confRefWithCRNExampleDoc)) mustBe false
      }
      "the document is held or locked with failure to delete the BR document" in new Setup {
        val confRefWithCRNExampleDoc: CorporationTaxRegistration = exampleDoc.copy(status = "held", confirmationReferences = Some(ConfirmationReferences("", "transID", None, None)))

        when(mockIncorpInfoConnector.checkCompanyIncorporated(any())(any()))
          .thenReturn(Future.successful(None))
        when(mockSubmissionEventService.topUpCTSubmission(any(), any(), any())(any()))
          .thenReturn(Future.successful(HttpResponse(200, "")))
        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.failed(new RuntimeException("failed to delete BR doc")))

        await(service.processStaleDocument(confRefWithCRNExampleDoc)) mustBe false
      }
      "the document is held or locked with failure to delete the CR document" in new Setup {
        val confRefWithCRNExampleDoc: CorporationTaxRegistration = exampleDoc.copy(status = "held", confirmationReferences = Some(ConfirmationReferences("", "transID", None, None)))

        when(mockIncorpInfoConnector.checkCompanyIncorporated(any())(any()))
          .thenReturn(Future.successful(None))
        when(mockSubmissionEventService.topUpCTSubmission(any(), any(), any())(any()))
          .thenReturn(Future.successful(HttpResponse(200, "")))
        when(mockBusRegConnector.adminRemoveMetadata(any()))
          .thenReturn(Future.successful(true))
        when(mockCorpTaxRegistrationRepo.removeTaxRegistrationById(any()))
          .thenReturn(Future.failed(new RuntimeException("failed to delete CR doc")))

        await(service.processStaleDocument(confRefWithCRNExampleDoc)) mustBe false
      }
    }
  }
}
