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
import play.api.Logging
import play.api.http.Status._
import play.api.libs.json.JsValue
import play.api.libs.ws.JsonBodyWritables.writeableOf_JsValue
import sttp.model.HeaderNames
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpErrorFunctions, HttpReads, HttpResponse, StringContextOps, UpstreamErrorResponse}

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.Base64
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

@Singleton
class HipConnector @Inject()(
                             appConfig: MicroserviceAppConfig,
                             httpClientV2: HttpClientV2 )(
  implicit ec: ExecutionContext) extends HttpErrorFunctions with Logging with CorrelationGenerator  {

  def ctSubmission(ackRef: String, submission: JsValue, journeyId: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = {

    val uri = s"${appConfig.hipUrl}/RESTAdapter/business-registration/CT"

    cPOST(uri, submission) map { response =>
        logger.info(s"[ctSubmission] Submission to HIP successful for regId: $journeyId AckRef: $ackRef")
        response
      } recoverWith {
        case ex: UpstreamErrorResponse if UpstreamErrorResponse.Upstream4xxResponse.unapply(ex).isDefined =>
          logger.error("HIP_SUBMISSION_400")
          logger.warn(s"[ctSubmission] Submission to HIP was invalid for regId: $journeyId AckRef: $ackRef")
          throw ex
      }
    }

  def topUpCTSubmission(ackRef: String, submission: JsValue, journeyId: String)(implicit headerCarrier: HeaderCarrier): Future[HttpResponse] = {
    val uri: String =s"${appConfig.hipUrl}/RESTAdapter/business-incorporation/CT"

    cPOST(uri, submission) map { response =>
      logger.info(s"[ctTopUpSubmission] Top up submission to HIP successful for regId: $journeyId AckRef: $ackRef")
      response
    } recoverWith {
      case ex: UpstreamErrorResponse if UpstreamErrorResponse.Upstream4xxResponse.unapply(ex).isDefined =>
        logger.error("HIP_SUBMISSION_400")
        logger.warn(s"[ctTopUpSubmission] Top up submission to HIP was invalid for regId: $journeyId AckRef: $ackRef")
        throw ex
    }
  }

  private def cPOST(uri: String, body: JsValue)(implicit hc: HeaderCarrier) = {

    val correlationId =
      addCorrelationId(hc).extraHeaders
        .map { case (key, value) => (key.toLowerCase, value) }
        .collectFirst { case ("correlationid", value) =>
          value
        }
        .getOrElse(generateCorrelationId(hc.requestId))

    val authSecret: String =
      Base64.getEncoder
        .encodeToString(
          s"${appConfig.hipClientId}:${appConfig.hipClientSecret}"
            .getBytes(StandardCharsets.UTF_8)
        )

    val hipHeaders: Seq[(String, String)] =
      Seq(
        HeaderNames.Authorization -> s"Basic $authSecret",
        "X-Originating-System"    -> "SCRS",
        "correlationid"           -> correlationId,
        "X-Receipt-Date"          -> DateTimeFormatter.ISO_INSTANT.format(Instant.now().truncatedTo(ChronoUnit.SECONDS)),
        "X-Transmitting-System"   -> "HIP"
      )

    httpClientV2
      .post(url"$uri")(hc)
      .setHeader(hipHeaders:_*)
      .withBody(body)
      .execute
  }

  implicit val httpRds: HttpReads[HttpResponse] = (http: String, url: String, response: HttpResponse) => response.status match {
    case CONFLICT =>
      logger.warn("[HipConnector httpRds]: Received 409 from HIP - converting to 202")
      HttpResponse(ACCEPTED, response.body, response.headers)
    case TOO_MANY_REQUESTS =>
      logger.warn("[HipConnector httpRds] Received 429 from HIP - converting to 503")
      throw UpstreamErrorResponse("TooManyRequests received from HIP submission", TOO_MANY_REQUESTS, SERVICE_UNAVAILABLE)
    case 499 =>
      logger.warn("[HipConnector httpRds] Received 499 from HIP - converting to 502")
      throw UpstreamErrorResponse("Timeout received from HIP submission", 499, BAD_GATEWAY)
    case status if is4xx(status) =>
      throw UpstreamErrorResponse(
        upstreamResponseMessage(http, url, status, response.body), status, reportAs = BAD_REQUEST, response.headers)
    case _ =>
      handleResponseEither(http, url)(response).fold(errorResponse => throw errorResponse, identity)
  }

}
