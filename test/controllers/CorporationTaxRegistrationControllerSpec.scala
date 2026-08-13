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

package controllers

import auth.CryptoSCRS
import fixtures.{CorporationTaxRegistrationFixture, CorporationTaxRegistrationResponse}
import helpers.BaseSpec
import mocks.{AuthorisationMocks, MockMetricsService}
import models.api.BusinessAddress
import models.validation.MongoValidation
import models.{CHROAddress, ConfirmationReferences, CorporationTaxRegistration, PPOBAddress}
import org.mockito.ArgumentMatchers.{any, eq => eqTo}
import org.mockito.Mockito._
import org.mockito.{ArgumentCaptor, ArgumentMatchers}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.{CorporationTaxRegistrationMongoRepository, Repositories}
import uk.gov.hmrc.auth.core.InsufficientConfidenceLevel
import utils.AlertLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CorporationTaxRegistrationControllerSpec extends BaseSpec with AuthorisationMocks with CorporationTaxRegistrationFixture {

  val mockRepositories: Repositories = mock[Repositories]
  val mockAlertLogging: AlertLogging = mock[AlertLogging]
  override val mockResource: CorporationTaxRegistrationMongoRepository = mockTypedResource[CorporationTaxRegistrationMongoRepository]

  class Setup {
    val controller: CorporationTaxRegistrationController =
      new CorporationTaxRegistrationController(
        MockMetricsService,
        mockAuthConnector,
        mockCTDataService,
        mockRepositories,
        mockAlertLogging,
        mockInstanceOfCrypto,
        stubControllerComponents()
      ) {
        override val cryptoSCRS: CryptoSCRS = mockInstanceOfCrypto
        override lazy val resource: CorporationTaxRegistrationMongoRepository = mockResource
      }
  }

  val regId = "reg-12345"
  val internalId = "int-12345"
  val authProviderId = "auth-prov-id-12345"

  "createCorporationTaxRegistration" must {

    val request = FakeRequest().withBody(Json.toJson(validCorporationTaxRegistrationRequest))

    "return a 201 when a new entry is created from the parsed json" in new Setup {
      mockAuthorise(Future.successful(Some(internalId)))
      mockGetInternalId(Future.successful(internalId))

      when(mockCTDataService.createCorporationTaxRegistrationRecord(eqTo(internalId), eqTo(regId), eqTo("en")))
        .thenReturn(Future.successful(draftCorporationTaxRegistration(regId)))
      val response: CorporationTaxRegistrationResponse = buildCTRegistrationResponse(regId)

      val result: Future[Result] = controller.createCorporationTaxRegistration(regId)(request)
      status(result) mustBe CREATED
      contentAsJson(result) mustBe Json.toJson(response)
    }

    "return a 403 when no internalId retrieved from Auth" in new Setup {
      mockAuthorise(Future.successful(None))

      val result: Future[Result] = controller.createCorporationTaxRegistration(regId)(request)
      status(result) mustBe FORBIDDEN
    }

    "return a 403 when the user is not authorised" in new Setup {
      mockAuthorise(Future.failed(InsufficientConfidenceLevel()))

      val result: Future[Result] = controller.createCorporationTaxRegistration(regId)(request)
      status(result) mustBe FORBIDDEN
    }
  }

  "retrieveCorporationTaxRegistration" must {

    val ctRegistrationResponse = buildCTRegistrationResponse(regId)

    "return a 200 and a CorporationTaxRegistration model is one is found" in new Setup {
      mockAuthorise(Future.successful(Some(internalId)))
      mockGetInternalId(Future.successful(internalId))

      when(mockCTDataService.retrieveCorporationTaxRegistrationRecord(eqTo(regId), any()))
        .thenReturn(Future.successful(Some(draftCorporationTaxRegistration(regId))))

      val result: Future[Result] = controller.retrieveCorporationTaxRegistration(regId)(FakeRequest())
      status(result) mustBe OK
      contentAsJson(result) mustBe Json.toJson(ctRegistrationResponse)
    }

    "return a 404 if a CT registration record cannot be found" in new Setup {
      mockAuthorise(Future.successful(Some(internalId)))
      mockGetInternalId(Future.successful(internalId))

      CTServiceMocks.retrieveCTDataRecord(regId, None)

      val result: Future[Result] = controller.retrieveCorporationTaxRegistration(regId)(FakeRequest())
      status(result) mustBe NOT_FOUND
    }

    "return a 403 when the user is not authenticated" in new Setup {
      mockAuthorise(Future.failed(InsufficientConfidenceLevel()))

      val result: Future[Result] = controller.retrieveCorporationTaxRegistration(regId)(FakeRequest())
      status(result) mustBe FORBIDDEN
    }
  }

  "retrieveFullCorporationTaxRegistration" must {

    "return a 200 and a CorporationTaxRegistration model is found" in new Setup {
      mockAuthorise(Future.successful(Some(internalId)))
      mockGetInternalId(Future.successful(internalId))

      CTServiceMocks.retrieveCTDataRecord(regId, Some(validDraftCorporationTaxRegistration))

      val result: Future[Result] = controller.retrieveFullCorporationTaxRegistration(regId)(FakeRequest())
      status(result) mustBe OK
      contentAsJson(result) mustBe Json.toJson(validDraftCorporationTaxRegistration)(CorporationTaxRegistration.format(MongoValidation, mockInstanceOfCrypto))
    }

    "return a 404 if a CT registration record cannot be found" in new Setup {
      mockAuthorise(Future.successful(Some(internalId)))
      mockGetInternalId(Future.successful(internalId))

      CTServiceMocks.retrieveCTDataRecord(regId, None)

      val result: Future[Result] = controller.retrieveFullCorporationTaxRegistration(regId)(FakeRequest())
      status(result) mustBe NOT_FOUND
    }

    "return a 403 when the user is not authenticated" in new Setup {
      mockAuthorise(Future.failed(InsufficientConfidenceLevel()))

      val result: Future[Result] = controller.retrieveFullCorporationTaxRegistration(regId)(FakeRequest())
      status(result) mustBe FORBIDDEN
    }
  }

  "retrieveConfirmationReference" must {

    "return a 200 and an acknowledgement ref is one exists" in new Setup {
      mockAuthorise(Future.successful(Some(internalId)))
      mockGetInternalId(Future.successful(internalId))

      val expected: ConfirmationReferences = ConfirmationReferences("BRCT00000000123", "tx", Some("py"), Some("12.00"))
      when(mockCTDataService.retrieveConfirmationReferences(ArgumentMatchers.eq(regId)))
        .thenReturn(Future.successful(Some(expected)))

      val result: Future[Result] = controller.retrieveConfirmationReference(regId)(FakeRequest())
      status(result) mustBe OK
      contentAsJson(result) mustBe Json.toJson(expected)
    }

    "return a 404 if a record cannot be found" in new Setup {
      mockAuthorise(Future.successful(Some(internalId)))
      mockGetInternalId(Future.successful(internalId))

      when(mockCTDataService.retrieveConfirmationReferences(ArgumentMatchers.eq(regId)))
        .thenReturn(Future.successful(None))

      val result: Future[Result] = controller.retrieveConfirmationReference(regId)(FakeRequest())
      status(result) mustBe NOT_FOUND
    }

    "return a 403 when the user is not authenticated" in new Setup {
      mockAuthorise(Future.failed(InsufficientConfidenceLevel()))

      val result: Future[Result] = controller.retrieveConfirmationReference(regId)(FakeRequest())
      status(result) mustBe FORBIDDEN
    }
  }

  "updateRegistrationProgress" must {

    def progressRequest(progress: String): JsValue = Json.parse(s"""{"registration-progress":"$progress"}""")

    "Extract the progress correctly from the message and request doc is updated" in new Setup {
      mockAuthorise(Future.successful(Some(internalId)))
      mockGetInternalId(Future.successful(internalId))

      val progress: String = "HO5"
      val request: FakeRequest[JsValue] = FakeRequest().withBody(progressRequest(progress))
      when(mockCTDataService.updateRegistrationProgress(ArgumentMatchers.eq(regId), ArgumentMatchers.any[String]())).
        thenReturn(Future.successful(Some("")))
      val response: Future[Result] = controller.updateRegistrationProgress(regId)(request)

      status(response) mustBe OK

      val captor: ArgumentCaptor[String] = ArgumentCaptor.forClass[String, String](classOf[String])
      verify(mockCTDataService, times(1)).updateRegistrationProgress(eqTo(regId), captor.capture())
      captor.getValue mustBe progress
    }

    "Return not found is the doc couldn't be updated" in new Setup {
      mockAuthorise(Future.successful(Some(internalId)))
      mockGetInternalId(Future.successful(internalId))

      val progress = "N/A"
      val request: FakeRequest[JsValue] = FakeRequest().withBody(progressRequest(progress))

      when(mockCTDataService.updateRegistrationProgress(eqTo(regId), any())).
        thenReturn(Future.successful(None))

      val result: Future[Result] = controller.updateRegistrationProgress(regId)(request)
      status(result) mustBe NOT_FOUND
    }
  }

  "convertAndReturnRoAddressIfValidInPPOBFormat" must {

    val cHROAddress = Json.toJson(CHROAddress("p", "14 St Test Walk", Some("Test"), "c", "l", Some("pb"), Some("TE1 1ST"), Some("r")))

    "return an OK if the RO address can be converted to a PPOB address" in new Setup {
      when(mockCTDataService.convertROToPPOBAddress(ArgumentMatchers.any()))
        .thenReturn(Some(PPOBAddress("test", "test", None, None, None, None, None, "test")))

      val request: FakeRequest[JsValue] = FakeRequest().withBody(cHROAddress)
      val response: Future[Result] = controller.convertAndReturnRoAddressIfValidInPPOBFormat()(request)

      status(response) mustBe OK
    }

    "return a Bad Request if the RO address cannot be converted to a PPOB address" in new Setup {
      when(mockCTDataService.convertROToPPOBAddress(ArgumentMatchers.any()))
        .thenReturn(None)

      val request: FakeRequest[JsValue] = FakeRequest().withBody(cHROAddress)
      val response: Future[Result] = controller.convertAndReturnRoAddressIfValidInPPOBFormat()(request)

      status(response) mustBe BAD_REQUEST
    }
  }

  "convertAndReturnRoAddressIfValidInBusinessAddressFormat" must {
    val anyCHROAddress = Json.toJson(
      CHROAddress(
        premises = "premises",
        address_line_1 = "14 St Test Walk",
        address_line_2 = Some("Test"),
        country = "country",
        locality = "locality",
        po_box = Some("po box"),
        postal_code = Some("TE1 1ST"),
        region = Some("region")
      ))
    "return an ok if the Ro can be converted to a Business Address" in new Setup {
      when(mockCTDataService.convertRoToBusinessAddress(ArgumentMatchers.any())).thenReturn(
        Some(BusinessAddress(
          line1 = "1 abc",
          line2 = "2 abc",
          line3 = Some("3 abc"),
          line4 = Some("4 abc"),
          postcode = Some("ZZ1 1ZZ"),
          country = Some("UK")
        ))
      )

      val request: FakeRequest[JsValue] = FakeRequest().withBody(anyCHROAddress)
      val response: Future[Result] = controller.convertAndReturnRoAddressIfValidInBusinessAddressFormat(request)
      status(response) mustBe OK
      contentAsJson(response) mustBe Json.parse(
        """
          |{
          |   "line1" : "1 abc",
          |   "line2" : "2 abc",
          |   "line3" : "3 abc",
          |   "line4" : "4 abc",
          |   "postcode" : "ZZ1 1ZZ",
          |   "country" : "UK"
          |}
        """.stripMargin
      )
    }
    "return bad request is ro address is invalid and cant be converted into a business address" in new Setup {
      when(mockCTDataService.convertRoToBusinessAddress(ArgumentMatchers.any())).thenReturn(
        None)

      val request: FakeRequest[JsValue] = FakeRequest().withBody(anyCHROAddress)
      val response: Future[Result] = controller.convertAndReturnRoAddressIfValidInBusinessAddressFormat(request)
      status(response) mustBe BAD_REQUEST
    }
  }
}