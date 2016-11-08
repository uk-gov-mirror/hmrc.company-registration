/*
 * Copyright 2016 HM Revenue & Customs
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

import config.WSHttp
import play.api.Logger
import play.api.http.Status._
import play.api.libs.json.{JsObject, Writes}
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._
import uk.gov.hmrc.play.http.logging.Authorization
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

sealed trait DesResponse
case class SuccessDesResponse(response: JsObject) extends DesResponse
case object NotFoundDesResponse extends DesResponse
case class InvalidDesRequest(message: String) extends DesResponse


trait DesConnector extends ServicesConfig with RawResponseReads {

  // $COVERAGE-OFF$
  lazy val serviceURL = baseUrl("des-service")
  // $COVERAGE-OFF$
  val baseURI = "/business-registration"
  val ctRegistrationURI = "/corporation-tax"

  val urlHeaderEnvironment: String
  val urlHeaderAuthorization: String

  val http: HttpGet with HttpPost with HttpPut = WSHttp

  def ctSubmission(ackRef:String, submission: JsObject)(implicit headerCarrier: HeaderCarrier): Future[DesResponse] = {
    val response = cPOST(s"""${serviceURL}${baseURI}${ctRegistrationURI}""", submission)
    response map { r =>
      r.status match {
        case OK => SuccessDesResponse(r.json.as[JsObject])
        case ACCEPTED => SuccessDesResponse(r.json.as[JsObject])
        case CONFLICT => {
          Logger.warn(s"ETMP reported a duplicate submission for ack ref ${ackRef}")
          SuccessDesResponse(r.json.as[JsObject])
        }
        case NOT_FOUND => NotFoundDesResponse
        case BAD_REQUEST => {
          val message = (r.json \ "reason").as[String]
          Logger.warn(s"ETMP reported an error with the request ${message}")
          InvalidDesRequest(message)
        }
        case _ => InvalidDesRequest(s"Unexpected Des response with HTTP code: ${r.status}")
      }
    }
  }


  private def createHeaderCarrier(headerCarrier: HeaderCarrier): HeaderCarrier = {
    headerCarrier.
      withExtraHeaders("Environment" -> urlHeaderEnvironment).
      copy(authorization = Some(Authorization(urlHeaderAuthorization)))
  }

  @inline
  private def cPOST[I, O](url: String, body: I, headers: Seq[(String, String)] = Seq.empty)(implicit wts: Writes[I], rds: HttpReads[O], hc: HeaderCarrier) =
    http.POST[I, O](url, body, headers)(wts = wts, rds = rds, hc = createHeaderCarrier(hc))

}

object DesConnector extends DesConnector {
  // $COVERAGE-OFF$
  val urlHeaderEnvironment: String = getConfString("des-service.environment", throw new Exception("could not find config value for des-service.environment"))
  val urlHeaderAuthorization: String = s"Bearer ${getConfString("des-service.authorization-token",
    throw new Exception("could not find config value for des-service.authorization-token"))}"
  // $COVERAGE-OFF$
}
