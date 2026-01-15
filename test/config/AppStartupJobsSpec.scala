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

package config

import assets.TestConstants.CorporationTaxRegistration.testTransactionId
import fixtures.CorporationTaxRegistrationFixture
import models.RegistrationStatus._
import models._
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.concurrent.Eventually
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.PlaySpec
import play.api.Configuration
import play.api.test.Helpers._
import repositories._
import services.{MetricsService, TakeoverDetailsService}
import utils.LogCapturingHelper

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class AppStartupJobsSpec extends PlaySpec with MockitoSugar with LogCapturingHelper with CorporationTaxRegistrationFixture with Eventually {

  val mockConfig: Configuration                                   = Configuration.empty
  val mockCTRepository: CorporationTaxRegistrationMongoRepository = mock[CorporationTaxRegistrationMongoRepository]
  val mockTakeoverDetailsService: TakeoverDetailsService          = mock[TakeoverDetailsService]
  val mockMetricsService: MetricsService                          = mock[MetricsService]

  val expectedLockedReg: Seq[Nothing] = List()
  val expectedRegStats                = Map.empty[String, Int]

  object TestAppStartupJobs extends AppStartupJobs {
    override val config: Configuration                             = mockConfig
    implicit val ec: ExecutionContext                              = global
    override val takeoverDetailsService: TakeoverDetailsService    = mockTakeoverDetailsService
    override val metricsService: MetricsService                    = mockMetricsService
    override val ctRepo: CorporationTaxRegistrationMongoRepository = mockCTRepository
    override def runEverythingOnStartUp(): Future[Unit]            = Future.successful(())
  }

  "get Company Name" must {

    val regId1       = "reg-1"
    val companyName1 = "ACME1 ltd"
    val companyName2 = "ACME2 ltd"

    val ctDoc1 = validCTRegWithCompanyName(regId1, companyName1)
    val ctDoc2 = validCTRegWithCompanyName(regId1, companyName2)

    "log specific company name relating to reg id passed in" in {
      when(mockCTRepository.retrieveLockedRegIDs())
        .thenReturn(Future.successful(expectedLockedReg))

      when(mockCTRepository.getRegistrationStats)
        .thenReturn(Future.successful(expectedRegStats))
      when(mockCTRepository.retrieveMultipleCorporationTaxRegistration(any()))
        .thenReturn(Future.successful(List(ctDoc1, ctDoc2)))

      withCaptureOfLoggingFrom(TestAppStartupJobs.logger) { logEvents =>
        eventually {
          await(TestAppStartupJobs.getCTCompanyName(regId1))
          val expectedLogs = List(
            s"[TestAppStartupJobs][getCTCompanyName] status : held - reg Id : $regId1 - Company Name : $companyName1 - Trans ID : $testTransactionId",
            s"[TestAppStartupJobs][getCTCompanyName] status : held - reg Id : $regId1 - Company Name : $companyName2 - Trans ID : $testTransactionId"
          )
          expectedLogs.diff(logEvents.map(_.getMessage)) mustBe List.empty
        }
      }
    }

  }
  "fetch Reg Ids" must {

    val dateTime: Instant = Instant.parse("2016-10-27T16:28:59.000Z")

    def corporationTaxRegistration(regId: String, status: String = SUBMITTED, transId: String = "transid-1"): CorporationTaxRegistration =
      CorporationTaxRegistration(
        internalId = "testID",
        registrationID = regId,
        formCreationTimestamp = dateTime.toString,
        language = LangConstants.english,
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
        status = status,
        confirmationReferences = Some(ConfirmationReferences(s"ACKFOR-$regId", transId, Some("PAYREF"), Some("12")))
      )

    val regIds = Seq("regId1", "regId2", "regId3")

    "log specific company name relating to reg id passed in" in {

      when(mockCTRepository.retrieveLockedRegIDs())
        .thenReturn(Future.successful(expectedLockedReg))

      when(mockCTRepository.getRegistrationStats)
        .thenReturn(Future.successful(expectedRegStats))
      when(mockCTRepository.findOneBySelector(mockCTRepository.regIDSelector("regId1")))
        .thenReturn(Future.successful(Some(corporationTaxRegistration("regId1", "TestStatus", "transid-1"))))
      when(mockCTRepository.findOneBySelector(mockCTRepository.regIDSelector("regId2")))
        .thenReturn(Future.successful(Some(corporationTaxRegistration("regId2", "TestStatus2", "transid-2"))))
      when(mockCTRepository.findOneBySelector(mockCTRepository.regIDSelector("regId3")))
        .thenReturn(Future.successful(None))

      withCaptureOfLoggingFrom(TestAppStartupJobs.logger) { logEvents =>
        eventually {
          await(TestAppStartupJobs.fetchDocInfoByRegId(regIds))

          logEvents.size mustBe 3
        }
      }
    }
  }
}
