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

import models.VatThreshold
import com.typesafe.config.{ConfigList, ConfigRenderOptions}
import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import play.api.Configuration
import play.api.libs.json.Json

@Singleton
class MicroserviceAppConfig @Inject()(servicesConfig: ServicesConfig, configuration: Configuration) {

  def getConfigString(key: String): String = servicesConfig.getConfString(key, throw new RuntimeException(s"Could not find $key in config"))

  private val thresholdString: String = configuration.get[ConfigList]("vat-threshold").render(ConfigRenderOptions.concise())
  val thresholds: Seq[VatThreshold]   = Json.parse(thresholdString).as[List[VatThreshold]]
  val threshold: Int             = servicesConfig.getConfInt("throttle-threshold", throw new Exception("throttle-threshold not found in config"))

  val regime: String     = getConfigString("regime")
  val subscriber: String = getConfigString("subscriber")

  val incorpInfoUrl: String = servicesConfig.baseUrl("incorporation-information")
  val compRegUrl: String    = servicesConfig.baseUrl("company-registration")

  lazy val useHip: Boolean         = servicesConfig.getBoolean("features.hip")
  lazy val desUrl: String          = servicesConfig.baseUrl("des-service")
  lazy val hipUrl: String          = servicesConfig.baseUrl("hip-connector")
  lazy val hipClientId: String     = servicesConfig.getString("microservice.services.hip.clientId")
  lazy val hipClientSecret: String = servicesConfig.getString("microservice.services.hip.clientSecret")
  lazy val apiRoute: String       = if (useHip) "HIP" else "DES"
}
