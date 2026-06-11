/*
 * Copyright 2026 HM Revenue & Customs
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

package connectors

import com.google.inject.Singleton
import config.MicroserviceAppConfig
import play.api.libs.json.{JsObject, Writes}
import uk.gov.hmrc.http.{HttpClient, _}
import utils.Logging

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class DesConnector @Inject()(
                              microserviceAppConfig: MicroserviceAppConfig,
                              http: HttpClient
                            )(
                              implicit ec: ExecutionContext) extends RawResponseReads with HttpErrorFunctions with Logging {

  private lazy val serviceURL: String = microserviceAppConfig.desUrl
  private lazy val urlHeaderEnvironment: String = microserviceAppConfig.getConfigString("des-service.environment")
  private lazy val urlHeaderAuthorization: String = s"Bearer ${microserviceAppConfig.getConfigString("des-service.authorization-token")}"

  private val baseURI = "/business-registration"
  private val baseTopUpURI = "/business-incorporation"
  private val ctRegistrationURI = "/corporation-tax"


  def ctSubmission(ackRef: String, submission: JsObject, journeyId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    val url: String = s"""$serviceURL$baseURI$ctRegistrationURI"""

    cPOST(url, submission) map { response =>
      logger.info(s"[ctSubmission] Submission to DES successful for regId: $journeyId AckRef: $ackRef")
      response
    } recoverWith {
      case ex: UpstreamErrorResponse if UpstreamErrorResponse.Upstream4xxResponse.unapply(ex).isDefined =>
        logger.error("DES_SUBMISSION_400")
        logger.warn(s"[ctSubmission] Submission to DES was invalid for regId: $journeyId AckRef: $ackRef")
        throw ex
    }
  }

  def topUpCTSubmission(ackRef: String, submission: JsObject, journeyId: String)(implicit headerCarrier: HeaderCarrier): Future[HttpResponse] = {
    val url: String = s"$serviceURL$baseTopUpURI$ctRegistrationURI"

    cPOST(url, submission) map { response =>
      logger.info(s"[ctTopUpSubmission] Top up submission to DES successful for regId: $journeyId AckRef: $ackRef")

      response
    } recoverWith {
      case ex: UpstreamErrorResponse if UpstreamErrorResponse.Upstream4xxResponse.unapply(ex).isDefined =>
        logger.error("DES_SUBMISSION_400")
        logger.warn(s"[ctTopUpSubmission] Top up submission to DES was invalid for regId: $journeyId AckRef: $ackRef")
        throw ex
    }
  }


  @inline
  private def cPOST[I, O](url: String, body: I)(implicit wts: Writes[I], rds: HttpReads[O], hc: HeaderCarrier) =
    http.POST[I, O](url, body, createHeaderCarrier())

  private[connectors] def createHeaderCarrier(): Seq[(String, String)] = {
    Seq(
      "Authorization" -> urlHeaderAuthorization,
      "Environment" -> urlHeaderEnvironment
    )
  }

  implicit val httpRds: HttpReads[HttpResponse] = new HttpReads[HttpResponse] {
    def read(http: String, url: String, res: HttpResponse): HttpResponse = customDESRead(http, url, res)
  }

  private[connectors] def customDESRead(http: String, url: String, response: HttpResponse): HttpResponse = {
    response.status match {
      case 409 =>
        logger.warn("[customDESRead] Received 409 from DES - converting to 202")
        HttpResponse(202, response.body, response.headers)
      case 429 =>
        logger.warn("[customDESRead] Received 429 from DES - converting to 503")
        throw UpstreamErrorResponse("TooManyRequests received from DES submission", 429, 503)
      case 499 =>
        logger.warn("[customDESRead] Received 499 from DES - converting to 502")
        throw UpstreamErrorResponse("Timeout received from DES submission", 499, 502)
      case status if is4xx(status) =>
        throw UpstreamErrorResponse(upstreamResponseMessage(http, url, status, response.body), status, reportAs = 400, response.headers)
      case _ =>
        handleResponseEither(http, url)(response).fold(errorResponse => throw errorResponse, identity)
    }
  }

}
