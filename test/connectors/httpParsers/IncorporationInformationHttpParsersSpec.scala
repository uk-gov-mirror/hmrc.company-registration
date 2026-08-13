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

package connectors.httpParsers

import ch.qos.logback.classic.Level
import helpers.BaseSpec
import play.api.http.Status._
import play.api.libs.json.Json
import uk.gov.hmrc.http.{HttpResponse, NotFoundException}
import utils.{AlertLogging, LogCapturingHelper}

class IncorporationInformationHttpParsersSpec extends BaseSpec with LogCapturingHelper with AlertLogging {

  val regId = "reg1234"
  val txId = "tx1234"
  val crn = "crn1234"

  "IncorporationInformationHttpParsers" when {

    "calling .registerInterestHttpParser" when {

      val rds = IncorporationInformationHttpParsers.registerInterestHttpParser(regId, txId)

      "response is ACCEPTED (202)" must {

        "return true and log an info msg" in {

          withCaptureOfLoggingFrom(IncorporationInformationHttpParsers.logger) { logs =>
            rds.read("", "", HttpResponse(ACCEPTED, "")) mustBe true
            logs.containsMsg(Level.INFO, s"[IncorporationInformationHttpParsers][registerInterestHttpParser] Registration forced returned 202 for regId: '$regId' and txId: '$txId'")
          }
        }
      }

      "response is any other status, e.g ISE" must {

        "return an Exception and log an error" in {

          val response = HttpResponse(INTERNAL_SERVER_ERROR, "")

          withCaptureOfLoggingFrom(IncorporationInformationHttpParsers.logger) { logs =>
            intercept[Exception](rds.read("", "/foo/bar", response))
            logs.containsMsg(Level.ERROR, s"[IncorporationInformationHttpParsers][registerInterestHttpParser] Calling url: '/foo/bar' returned unexpected status: '$INTERNAL_SERVER_ERROR'")
          }
        }
      }
    }

    "calling .cancelSubscriptionHttpParser" when {

      val rds = IncorporationInformationHttpParsers.cancelSubscriptionHttpParser(regId, txId)

      "response is OK (200)" must {

        "return true and log an info msg" in {

          withCaptureOfLoggingFrom(IncorporationInformationHttpParsers.logger) { logs =>
            rds.read("", "", HttpResponse(OK, "")) mustBe true
            logs.containsMsg(Level.INFO, s"[IncorporationInformationHttpParsers][cancelSubscriptionHttpParser] Cancelled subscription for regId: '$regId' and txId: '$txId'")
          }
        }
      }

      "response is NOT_FOUND (404)" must {

        "throw a NotFoundException" in {

          withCaptureOfLoggingFrom(IncorporationInformationHttpParsers.logger) { logs =>
            intercept[NotFoundException](rds.read("", "", HttpResponse(NOT_FOUND, "")))

            logs.containsMsg(Level.INFO, s"[IncorporationInformationHttpParsers][cancelSubscriptionHttpParser] No subscription to cancel for regId: '$regId' and txId: '$txId'")
          }
        }
      }

      "response is any other status, e.g ISE" must {

        "return an Exception and log an error" in {

          val response = HttpResponse(INTERNAL_SERVER_ERROR, "")

          withCaptureOfLoggingFrom(IncorporationInformationHttpParsers.logger) { logs =>
            intercept[Exception](rds.read("", "/foo/bar", response))
            logs.containsMsg(Level.ERROR, s"[IncorporationInformationHttpParsers][cancelSubscriptionHttpParser] Calling url: '/foo/bar' returned unexpected status: '$INTERNAL_SERVER_ERROR'")
          }
        }
      }
    }

    "calling .checkCompanyIncorporatedHttpParser" when {

      val rds = IncorporationInformationHttpParsers.checkCompanyIncorporatedHttpParser

      "response is OK (200) and JSON is valid" must {

        "return the CRN and log a PagerDuty" in {

          withCaptureOfLoggingFrom(IncorporationInformationHttpParsers.logger) { logs =>
            rds.read("", "", HttpResponse(OK, json = Json.obj("crn" -> crn), Map())) mustBe Some(crn)

            val logLevel = if (inWorkingHours) Level.ERROR else Level.INFO
            logs.containsMsg(logLevel, s"[IncorporationInformationHttpParsers] STALE_DOCUMENTS_DELETE_WARNING_CRN_FOUND")
          }
        }
      }

      "response is OK (200) no CRN exists" must {

        "return None" in {
          rds.read("", "", HttpResponse(OK, json = Json.obj(), Map())) mustBe None
        }
      }

      "response is NO_CONTENT (204)" must {

        "return None" in {
          rds.read("", "", HttpResponse(NO_CONTENT, "")) mustBe None
        }
      }

      "response is any other status, e.g ISE" must {

        "return an Exception and log an error" in {

          val response = HttpResponse(INTERNAL_SERVER_ERROR, "")

          withCaptureOfLoggingFrom(IncorporationInformationHttpParsers.logger) { logs =>
            intercept[Exception](rds.read("", "/foo/bar", response))
            logs.containsMsg(Level.ERROR, s"[IncorporationInformationHttpParsers][checkCompanyIncorporatedHttpParser] Calling url: '/foo/bar' returned unexpected status: '$INTERNAL_SERVER_ERROR'")
          }
        }
      }
    }
  }
}
