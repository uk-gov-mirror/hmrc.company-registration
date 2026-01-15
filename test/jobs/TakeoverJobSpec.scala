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

package jobs

import config.AppStartupJobs
import models.RegistrationStatus._
import models._
import org.mockito.Mockito._
import org.scalatest.concurrent.Eventually
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.PlaySpec
import play.api.Configuration
import play.api.test.Helpers._
import repositories._
import services.admin.AdminServiceImpl
import services.{MetricsService, TakeoverDetailsService}
import utils.Logging

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class TakeoverJobSpec extends PlaySpec with MockitoSugar with Logging with Eventually {

  val mockConfig: Configuration                                   = Configuration.empty
  val mockCTRepository: CorporationTaxRegistrationMongoRepository = mock[CorporationTaxRegistrationMongoRepository]
  val mockAdminService: AdminServiceImpl                          = mock[AdminServiceImpl]
  val mockTakeoverDetailsService: TakeoverDetailsService          = mock[TakeoverDetailsService]
  val mockMetricsService: MetricsService                          = mock[MetricsService]
  val expectedLockedReg: List[Nothing]                            = List()
  val expectedRegStats                                            = Map.empty[String, Int]

  "updateTakeoverData" should {

    val dateTime: LocalDateTime = LocalDateTime.parse("2016-10-27T16:28:59.000")

    def takeOverCorporationTaxRegistration(regId: String, incompleteTakeoverBlock: Boolean): CorporationTaxRegistration = {

      def takeoverBlock: Option[TakeoverDetails] =
        if (incompleteTakeoverBlock) {
          Some(TakeoverDetails(replacingAnotherBusiness = true, None, None, None, None))
        } else {
          Some(
            TakeoverDetails(
              replacingAnotherBusiness = true,
              Some("Takeover company name ltd"),
              Some(Address("Line1", "line2", Some("line3"), Some("line4"), Some("ZZ1 1ZZ"), None)),
              Some("Takeover name"),
              Some(Address("Line1", "line2", Some("line3"), Some("line4"), Some("ZZ1 1ZZ"), None))
            ))
        }

      CorporationTaxRegistration(
        internalId = "testID",
        registrationID = regId,
        formCreationTimestamp = dateTime.toString,
        language = "en",
        companyDetails = Some(
          CompanyDetails(
            "testCompanyName",
            CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
            PPOB(
              "MANUAL",
              Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), None, None, "txid"))),
            "testJurisdiction"
          )),
        contactDetails = Some(
          ContactDetails(
            Some("0123456789"),
            Some("0123456789"),
            Some("test@email.co.uk")
          )),
        tradingDetails = Some(TradingDetails("false")),
        status = DRAFT,
        confirmationReferences = Some(ConfirmationReferences(s"ACKFOR-$regId", s"transid-$regId", None, None)),
        takeoverDetails = takeoverBlock
      )
    }

    val regIds = List("regId2", "regId3")

    "only update the records with an invalid takeover block" in {

      val appStartupJobs: AppStartupJobs = new AppStartupJobs {
        override val config: Configuration                             = mockConfig
        implicit val ec: ExecutionContext                              = global
        override val takeoverDetailsService: TakeoverDetailsService    = mockTakeoverDetailsService
        override val metricsService: MetricsService                    = mockMetricsService
        override val ctRepo: CorporationTaxRegistrationMongoRepository = mockCTRepository

        override def runEverythingOnStartUp(): Future[Unit] = Future.successful(())

      }

      when(mockTakeoverDetailsService.updateTakeoverDetailsBlock("regId2", TakeoverDetails(replacingAnotherBusiness = false, None, None, None, None)))
        .thenReturn(Future.successful(TakeoverDetails(replacingAnotherBusiness = false, None, None, None, None)))
      when(mockTakeoverDetailsService.updateTakeoverDetailsBlock("regId3", TakeoverDetails(replacingAnotherBusiness = false, None, None, None, None)))
        .thenReturn(Future.successful(TakeoverDetails(replacingAnotherBusiness = false, None, None, None, None)))

      val expectedResult = List(
        TakeoverDetails(replacingAnotherBusiness = false, None, None, None, None),
        TakeoverDetails(replacingAnotherBusiness = false, None, None, None, None))
      await(appStartupJobs.updateTakeoverData(regIds)) mustBe expectedResult
      verify(mockTakeoverDetailsService).updateTakeoverDetailsBlock(
        "regId2",
        TakeoverDetails(replacingAnotherBusiness = false, None, None, None, None))
      verify(mockTakeoverDetailsService).updateTakeoverDetailsBlock(
        "regId3",
        TakeoverDetails(replacingAnotherBusiness = false, None, None, None, None))
    }
  }
}
