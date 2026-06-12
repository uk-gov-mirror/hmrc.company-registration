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

import config.LangConstants
import connectors._
import fixtures.CorporationTaxRegistrationFixture
import helpers.BaseSpec
import mocks.AuthorisationMocks
import models.RegistrationStatus._
import models._
import models.des.BusinessAddress
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito._
import org.scalatest.concurrent.Eventually
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http.{HeaderCarrier, SessionId}
import uk.gov.hmrc.mongo.lock.LockService
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import utils.LogCapturingHelper

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

class CorporationTaxRegistrationServiceSpec extends BaseSpec with AuthorisationMocks with LogCapturingHelper with Eventually {

  implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(SessionId("testSessionId")))
  implicit val req: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/test-path")

  val mockBRConnector: BusinessRegistrationConnector = mock[BusinessRegistrationConnector]
  val mockAuditConnector: AuditConnector = mock[AuditConnector]
  val mockIIConnector: IncorporationInformationConnector = mock[IncorporationInformationConnector]
  val mockSubmissionEventService: SubmissionEventService = mock[SubmissionEventService]

  val dateTime: Instant = Instant.parse("2016-10-27T16:28:59.000Z")

  val regId = "reg-id-12345"
  val transId = "trans-id-12345"
  val timestamp = "2016-10-27T17:06:23.000Z"
  val authProviderId = "auth-prov-id-12345"

  implicit val isAdmin: Boolean = false

  override protected def beforeEach(): Unit = {
    reset(
      mockCTDataRepository, mockSequenceMongoRepository, mockAuthConnector, mockBRConnector,
      mockIncorporationCheckAPIConnector, mockAuditConnector, mockIIConnector, mockSubmissionEventService, mockLockService
    )
  }

  class Setup extends CorporationTaxRegistrationFixture {
    val service: CorporationTaxRegistrationService = new CorporationTaxRegistrationService {
      val cTRegistrationRepository: CorporationTaxRegistrationMongoRepository = mockCTDataRepository
      val sequenceRepository: SequenceMongoRepository = mockSequenceMongoRepository
      val microserviceAuthConnector: AuthConnector = mockAuthConnector
      val brConnector: BusinessRegistrationConnector = mockBRConnector
      val submissionCheckAPIConnector: IncorporationCheckAPIConnector = mockIncorporationCheckAPIConnector
      val auditConnector: AuditConnector = mockAuditConnector
      val incorpInfoConnector: IncorporationInformationConnector = mockIIConnector
      val submissionEventService: SubmissionEventService = mockSubmissionEventService
      val instantNow: Instant = dateTime
      override val lockKeeper: LockService = mockLockService
      implicit val ec: ExecutionContext = global
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

  def partialEtmpSubmission(ackRef: String, timestamp: String = "2016-10-27T17:06:23.000Z"): JsObject = Json.parse(
    s"""
       |{
       | "acknowledgementReference":"$ackRef",
       | "registration":{
       |   "metadata":{
       |     "businessType":"Limited company",
       |     "submissionFromAgent":false,
       |     "declareAccurateAndComplete":true,
       |     "sessionId":"session-40fdf8c0-e2b1-437c-83b5-8689c2e1bc43",
       |     "credentialId":"cred-id-543212311772",
       |     "language":"en",
       |     "formCreationTimestamp":"$timestamp",
       |     "completionCapacity":"Other",
       |     "completionCapacityOther":"director"
       |   },
       |   "corporationTax":{
       |     "companyOfficeNumber":"001",
       |     "hasCompanyTakenOverBusiness":false,
       |     "companyMemberOfGroup":false,
       |     "companiesHouseCompanyName":"testCompanyName",
       |     "returnsOnCT61":false,
       |     "companyACharity":false,
       |     "businessAddress":{
       |       "line1":"",
       |       "line2":"",
       |       "line3":null,
       |       "line4":null,
       |       "postcode":null,
       |       "country":null
       |     },
       |     "businessContactDetails":{
       |       "telephoneNumber":"123",
       |       "mobileNumber":"123",
       |       "emailAddress":"6email@whatever.com"
       |     }
       |   }
       | }
       |}
      """.stripMargin).as[JsObject]

  "createCorporationTaxRegistrationRecord" must {

    "create a new ctData record and return a 201 - Created response" in new Setup {
      CTDataRepositoryMocks.createCorporationTaxRegistration(validDraftCorporationTaxRegistration)

      val result: Future[CorporationTaxRegistration] = service.createCorporationTaxRegistrationRecord("54321", "12345", "en")
      await(result) mustBe validDraftCorporationTaxRegistration
    }
  }

  "retrieveCorporationTaxRegistrationRecord" must {

    "return Corporation Tax registration response Json and a 200 - Ok when a record is retrieved" in new Setup {
      CTDataRepositoryMocks.retrieveCorporationTaxRegistration(Some(validDraftCorporationTaxRegistration))

      val result: Future[Option[CorporationTaxRegistration]] = service.retrieveCorporationTaxRegistrationRecord("testRegID")
      await(result) mustBe Some(validDraftCorporationTaxRegistration)
    }

    "return a 404 - Not found when no record is retrieved" in new Setup {
      CTDataRepositoryMocks.retrieveCorporationTaxRegistration(None)

      val result: Future[Option[CorporationTaxRegistration]] = service.retrieveCorporationTaxRegistrationRecord("testRegID")
      await(result) mustBe None
    }
  }

  "retrieveConfirmationReference" must {

    "return an refs if found" in new Setup {
      val expected: ConfirmationReferences = ConfirmationReferences("testTransaction", "testPayRef", Some("testPayAmount"), Some("12"))

      when(mockCTDataRepository.retrieveConfirmationReferences(eqTo(regId)))
        .thenReturn(Future.successful(Some(expected)))

      val result: Option[ConfirmationReferences] = await(service.retrieveConfirmationReferences(regId))
      result mustBe Some(expected)
    }

    "return an empty option if an Ack ref is not found" in new Setup {
      when(mockCTDataRepository.retrieveConfirmationReferences(eqTo(regId)))
        .thenReturn(Future.successful(None))

      val result: Option[ConfirmationReferences] = await(service.retrieveConfirmationReferences(regId))
      result mustBe None
    }
  }


  "locateOldHeldSubmissions" must {
    val registrationId = "testRegId"
    val tID = "transID"
    val heldTime = Some(Instant.now().minus(7, ChronoUnit.DAYS))

    val oldHeldSubmission = CorporationTaxRegistration(
      internalId = "testID",
      registrationID = registrationId,
      formCreationTimestamp = dateTime.toString,
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
      heldTimestamp = heldTime,
      confirmationReferences = Some(ConfirmationReferences(
        acknowledgementReference = "ackRef",
        transactionId = tID,
        paymentReference = Some("payref"),
        paymentAmount = Some("12")
      ))
    )

    "log nothing and return 'No week old held submissions found' in said case" in new Setup {
      when(mockCTDataRepository.retrieveAllWeekOldHeldSubmissions())
        .thenReturn(Future.successful(List()))

      val result: String = await(service.locateOldHeldSubmissions)
      result mustBe "No week old held submissions found"
    }

    "log cases of week old held submissions and output 'Week old held submissions found'" in new Setup {
      withCaptureOfLoggingFrom(service.logger) { logEvents =>
        when(mockCTDataRepository.retrieveAllWeekOldHeldSubmissions())
          .thenReturn(Future.successful(List(oldHeldSubmission)))

        val result = await(service.locateOldHeldSubmissions)
        result mustBe "Week old held submissions found"

        eventually {
          logEvents.length mustBe 2
          logEvents.head.getMessage must include("ALERT_missing_incorporations")
          logEvents.tail.head.getMessage must
            include(s"Held submission older than one week of regID: $registrationId txID: $tID heldDate: ${heldTime.get.toString})")
        }
      }
    }
  }


  "convertROToPPOBAddress" must {
    val premise = "pr"
    val country = "Testland"
    val local = "locality"
    val pobox = Some("pobox")
    val testPost = Some("ZZ1 1ZZ")
    val region = Some("region")

    val characterConverts = Map('æ' -> "ae", 'Æ' -> "AE", 'œ' -> "oe", 'Œ' -> "OE", 'ß' -> "ss", 'ø' -> "o", 'Ø' -> "O")
    val concatenatedCharacters = characterConverts.keySet.mkString

    "convert a RO address to a PPOB address" when {

      "the RO address is valid with no special characters" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          premise, "-1 Test Road", Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        service.convertROToPPOBAddress(roAddress) mustBe Some(PPOBAddress(
          premise + " " + roAddress.address_line_1,
          roAddress.address_line_2.get,
          Some(local),
          region,
          roAddress.postal_code,
          Some(roAddress.country),
          None,
          ""
        ))
      }

      "the RO address line 1 contains an accented character" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          premise, "-1 Tést Road", Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        service.convertROToPPOBAddress(roAddress) mustBe Some(PPOBAddress(
          premise + " " + "-1 Test Road",
          roAddress.address_line_2.get,
          Some(local),
          region,
          roAddress.postal_code,
          Some(roAddress.country),
          None,
          ""
        ))
      }

      "the RO address line 1 contains unexpected punctation" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          premise, "-1 Test![][@~~ Road", Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        service.convertROToPPOBAddress(roAddress) mustBe Some(PPOBAddress(
          premise + " " + "-1 Test Road",
          roAddress.address_line_2.get,
          Some(local),
          region,
          roAddress.postal_code,
          Some(roAddress.country),
          None,
          ""
        ))
      }

      s"the RO address line 1 contains $concatenatedCharacters" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          premise, s"-1 Test $concatenatedCharacters", Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        val expectedConvertedConcat: String = concatenatedCharacters map characterConverts mkString

        service.convertROToPPOBAddress(roAddress) mustBe Some(PPOBAddress(
          premise + " " + s"-1 Test $expectedConvertedConcat",
          roAddress.address_line_2.get,
          Some(local),
          region,
          roAddress.postal_code,
          Some(roAddress.country),
          None,
          ""
        ))
      }

      "the RO address has more than 27 characters" in new Setup {
        val stringOf27Chars: String = List.fill(25)("a").mkString

        val roAddress: CHROAddress = CHROAddress(
          premise, stringOf27Chars, Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        service.convertROToPPOBAddress(roAddress) mustBe Some(PPOBAddress(
          premise + " " + stringOf27Chars.take(24),
          roAddress.address_line_2.get,
          Some(local),
          region,
          roAddress.postal_code,
          Some(roAddress.country),
          None,
          ""
        ))
      }

      "the RO address expands beyond after converting characters" in new Setup {
        val twentyPlusConcat: String = List.fill(23 - concatenatedCharacters.length)("a").mkString + concatenatedCharacters
        twentyPlusConcat.length mustBe 23

        val roAddress: CHROAddress = CHROAddress(
          premise, twentyPlusConcat, Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        service.convertROToPPOBAddress(roAddress) mustBe Some(PPOBAddress(
          (premise + " " + twentyPlusConcat.map(char => characterConverts.getOrElse(char, char)).mkString).take(27),
          roAddress.address_line_2.get,
          Some(local),
          region,
          roAddress.postal_code,
          Some(roAddress.country),
          None,
          ""
        ))
      }

      "the RO address contains no address line 2" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          premise, "-1 Test Road", None, country, local, pobox, None, region
        )

        service.convertROToPPOBAddress(roAddress) mustBe Some(PPOBAddress(
          (premise + " " + "-1 Test Road").take(27),
          local,
          region,
          None,
          roAddress.postal_code,
          Some(roAddress.country),
          None,
          ""
        ))
      }

    }
    "fail to convert" when {
      "the RO address contains only a pipe character in address line 1" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          "", "|", Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        service.convertROToPPOBAddress(roAddress) mustBe None
      }
    }

    "fail to convert" when {
      "the RO address contains a pipe in the post code" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          ">", "Test Two", Some("-1 Test Town"), country, local, pobox, Some("|"), region
        )

        service.convertROToPPOBAddress(roAddress) mustBe None
      }
    }
  }
  "convertROToBusinessAddress" must {
    val premise = "pr"
    val country = "Testland"
    val local = "locality"
    val pobox = Some("pobox")
    val testPost = Some("ZZ1 1ZZ")
    val region = Some("region")

    val characterConverts = Map('æ' -> "ae", 'Æ' -> "AE", 'œ' -> "oe", 'Œ' -> "OE", 'ß' -> "ss", 'ø' -> "o", 'Ø' -> "O")
    val concatenatedCharacters = characterConverts.keySet.mkString

    "convert a RO address to a BusinessAddress address" when {

      "the RO address is valid with no special characters" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          premise, "-1 Test Road", Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        service.convertRoToBusinessAddress(roAddress) mustBe Some(BusinessAddress(
          premise + " " + roAddress.address_line_1,
          roAddress.address_line_2.get,
          Some(local),
          region,
          roAddress.postal_code,
          Some(roAddress.country)
        ))
      }

      "the RO address line 1 contains an accented character" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          premise, "-1 Tést Road", Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        service.convertRoToBusinessAddress(roAddress) mustBe Some(BusinessAddress(
          premise + " " + "-1 Test Road",
          roAddress.address_line_2.get,
          Some(local),
          region,
          roAddress.postal_code,
          Some(roAddress.country)
        ))
      }

      "the RO address line 1 contains unexpected punctation" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          premise, "-1 Test![][@~~ Road", Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        service.convertRoToBusinessAddress(roAddress) mustBe Some(BusinessAddress(
          premise + " " + "-1 Test Road",
          roAddress.address_line_2.get,
          Some(local),
          region,
          roAddress.postal_code,
          Some(roAddress.country)
        ))
      }

      s"the RO address line 1 contains $concatenatedCharacters" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          premise, s"-1 Test $concatenatedCharacters", Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        val expectedConvertedConcat: String = concatenatedCharacters map characterConverts mkString

        service.convertRoToBusinessAddress(roAddress) mustBe Some(BusinessAddress(
          premise + " " + s"-1 Test $expectedConvertedConcat",
          roAddress.address_line_2.get,
          Some(local),
          region,
          roAddress.postal_code,
          Some(roAddress.country)
        ))
      }

      "the RO address has more than 27 characters" in new Setup {
        val stringOf27Chars: String = List.fill(25)("a").mkString

        val roAddress: CHROAddress = CHROAddress(
          premise, stringOf27Chars, Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        service.convertRoToBusinessAddress(roAddress) mustBe Some(BusinessAddress(
          premise + " " + stringOf27Chars.take(24),
          roAddress.address_line_2.get,
          Some(local),
          region,
          roAddress.postal_code,
          Some(roAddress.country)
        ))
      }

      "the RO address expands beyond after converting characters" in new Setup {
        val twentyPlusConcat: String = List.fill(23 - concatenatedCharacters.length)("a").mkString + concatenatedCharacters
        twentyPlusConcat.length mustBe 23

        val roAddress: CHROAddress = CHROAddress(
          premise, twentyPlusConcat, Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        service.convertRoToBusinessAddress(roAddress) mustBe Some(BusinessAddress(
          (premise + " " + twentyPlusConcat.map(char => characterConverts.getOrElse(char, char)).mkString).take(27),
          roAddress.address_line_2.get,
          Some(local),
          region,
          roAddress.postal_code,
          Some(roAddress.country)
        ))
      }

      "the RO address contains no address line 2" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          premise, "-1 Test Road", None, country, local, pobox, None, region
        )

        service.convertRoToBusinessAddress(roAddress) mustBe Some(BusinessAddress(
          (premise + " " + "-1 Test Road").take(27),
          local,
          region,
          None,
          roAddress.postal_code,
          Some(roAddress.country)
        ))
      }

    }
    "fail to convert" when {
      "the RO address contains only a pipe character in address line 1" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          "", "|", Some("-1 Test Town"), country, local, pobox, testPost, region
        )

        service.convertRoToBusinessAddress(roAddress) mustBe None
      }
    }

    "fail to convert" when {
      "the RO address contains a pipe in the post code" in new Setup {
        val roAddress: CHROAddress = CHROAddress(
          ">", "Test Two", Some("-1 Test Town"), country, local, pobox, Some("|"), region
        )

        service.convertRoToBusinessAddress(roAddress) mustBe None
      }
    }
  }
}