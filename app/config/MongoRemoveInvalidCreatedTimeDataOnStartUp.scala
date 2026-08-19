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

package config

import org.apache.pekko.actor.ActorSystem
import repositories.CorporationTaxRegistrationMongoRepository
import utils.Logging

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{DurationInt, FiniteDuration}

@Singleton
class MongoRemoveInvalidCreatedTimeDataOnStartUp @Inject() (
                                                             actorSystem: ActorSystem,
                                                             ctrRepository: CorporationTaxRegistrationMongoRepository,
                                                             cleanupConfig: CreatedTimeDataCleanupConfig
                                                           )(implicit ec: ExecutionContext)
  extends Logging {

  protected def jitterDelay: FiniteDuration = (10 + scala.util.Random.nextInt(5)).seconds

  actorSystem.scheduler.scheduleOnce(jitterDelay) {
    logger.warn(s"[MongoRemoveInvalidCreatedTimeData] Start up job has started after delay of $jitterDelay.")
    deleteInvalidData()
  }

  def deleteInvalidData(): Unit = {
    val deleteAllDocuments: Boolean      = cleanupConfig.deleteAllInvalidCreatedTimeData
    val deleteNDocuments: Boolean        = cleanupConfig.deleteNInvalidCreatedTimeData
    val deleteDocumentLimit: Int         = cleanupConfig.limitForDeletingInvalidCreatedTimeData

    (deleteNDocuments, deleteDocumentLimit, deleteAllDocuments) match {
      case (true, limit, false) if limit > 0 =>
        logger.warn(
          s"[MongoRemoveInvalidCreatedTimeData] 'deleteNInvalidCreatedTimeData' config is set to true - starting deletion of stale legacy createdTime data."
        )
        ctrRepository.deleteNStaleLegacyCreatedTimeData(limit)
      case (false, _, true) =>
        logger.warn(
          s"[MongoRemoveInvalidCreatedTimeData] 'deleteAllInvalidCreatedTimeData' config is set to true - starting deletion of all stale legacy createdTime data."
        )
        ctrRepository.deleteAllStaleLegacyCreatedTimeData()
      case (true, limit, false) =>
        logger.warn(
          s"[MongoRemoveInvalidCreatedTimeData] 'deleteNInvalidCreatedTimeData' switch is on but limit config is invalid" +
            s" - no action taken."
        )
      case (false, _, false) =>
        logger.warn(
          s"[MongoRemoveInvalidCreatedTimeData] 'deleteAllInvalidCreatedTimeData' and 'deleteNInvalidCreatedTimeData'" +
            s" configs are both false - no action taken."
        )
      case _ =>
        logger.warn(
          s"[MongoRemoveInvalidCreatedTimeData] Conflicting 'deleteAllInvalidCreatedTimeData' / 'deleteNInvalidCreatedTimeData'" +
            s" configs - no action taken."
        )
    }
  }
}