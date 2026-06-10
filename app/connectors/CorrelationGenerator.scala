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

import uk.gov.hmrc.http.{HeaderCarrier, RequestId}

import java.util.UUID.randomUUID

trait CorrelationGenerator { // scalastyle: off magic.number

  val CorrelationIdHeaderName = "CorrelationId"

  def generateRandomUUID: String = randomUUID.toString

  def addCorrelationId(hc: HeaderCarrier): HeaderCarrier = { // scalaStyle:off magic.number

    val correlationId: String = generateCorrelationId(hc.requestId)
    hc.withExtraHeaders(CorrelationIdHeaderName -> correlationId)
  }

  def generateCorrelationId(requestId: Option[RequestId]): String = {
    val CorrelationIdPattern = """.*([A-Za-z0-9]{8}-[A-Za-z0-9]{4}-[A-Za-z0-9]{4}-[A-Za-z0-9]{4}).*""".r
    val correlationId        = requestId match {
      case Some(requestId) =>
        requestId.value match {
          case CorrelationIdPattern(prefix) => prefix + "-" + generateRandomUUID.substring(24)
          case _                            => generateRandomUUID
        }
      case _               => generateRandomUUID
    }
    correlationId
  }

}
