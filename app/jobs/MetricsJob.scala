/*
 * Copyright 2017 HM Revenue & Customs
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

import javax.inject.Inject

import play.modules.reactivemongo.MongoDbConnection
import reactivemongo.api.DefaultDB
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.scheduling.ExclusiveScheduledJob
import utils.SCRSFeatureSwitches

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MetricsJobImpl @Inject()() extends MetricsJob {
  val name = "metrics-job"
  lazy val db: () => DefaultDB = new MongoDbConnection{}.db
}

trait MetricsJob extends ExclusiveScheduledJob with JobConfig with JobHelper {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  override def executeInMutex(implicit ec: ExecutionContext): Future[Result] = {
    ifFeatureEnabled(SCRSFeatureSwitches.graphiteMetrics) {
      ???
    }
  }
}