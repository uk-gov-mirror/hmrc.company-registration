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

import com.mongodb.client.result.DeleteResult
import com.typesafe.config.ConfigFactory
import org.apache.pekko.actor.ActorSystem
import org.mockito.ArgumentMatchers.anyInt
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.PlaySpec
import repositories.CorporationTaxRegistrationMongoRepository

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class MongoRemoveInvalidCreatedTimeDataOnStartUpSpec
  extends PlaySpec
    with MockitoSugar
    with BeforeAndAfterEach {

  private val mockCtrRepository: CorporationTaxRegistrationMongoRepository =
    mock[CorporationTaxRegistrationMongoRepository]
  private val mockCleanupConfig: CreatedTimeDataCleanupConfig =
    mock[CreatedTimeDataCleanupConfig]
  private val testActorSystem: ActorSystem = ActorSystem("testActorSystem", ConfigFactory.load())

  class TestStartUpJob
    extends MongoRemoveInvalidCreatedTimeDataOnStartUp(
      testActorSystem,
      mockCtrRepository,
      mockCleanupConfig
    )

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockCtrRepository, mockCleanupConfig)
  }

  "deleteInvalidData" should {
    "not start deletion process" when {
      "both delete flags are disabled" in {
        when(mockCleanupConfig.deleteAllInvalidCreatedTimeData).thenReturn(false)
        when(mockCleanupConfig.deleteNInvalidCreatedTimeData).thenReturn(false)
        when(mockCleanupConfig.limitForDeletingInvalidCreatedTimeData).thenReturn(1)

        new TestStartUpJob().deleteInvalidData()

        verify(mockCtrRepository, never()).deleteAllStaleLegacyCreatedTimeData()
        verify(mockCtrRepository, never()).deleteNStaleLegacyCreatedTimeData(anyInt())
      }

      "deleteNInvalidCreatedTimeData is enabled but limit is zero" in {
        when(mockCleanupConfig.deleteAllInvalidCreatedTimeData).thenReturn(false)
        when(mockCleanupConfig.deleteNInvalidCreatedTimeData).thenReturn(true)
        when(mockCleanupConfig.limitForDeletingInvalidCreatedTimeData).thenReturn(0)

        new TestStartUpJob().deleteInvalidData()

        verify(mockCtrRepository, never()).deleteNStaleLegacyCreatedTimeData(anyInt())
        verify(mockCtrRepository, never()).deleteAllStaleLegacyCreatedTimeData()
      }

      "both delete flags are enabled (conflicting)" in {
        when(mockCleanupConfig.deleteAllInvalidCreatedTimeData).thenReturn(true)
        when(mockCleanupConfig.deleteNInvalidCreatedTimeData).thenReturn(true)
        when(mockCleanupConfig.limitForDeletingInvalidCreatedTimeData).thenReturn(10)

        new TestStartUpJob().deleteInvalidData()

        verify(mockCtrRepository, never()).deleteAllStaleLegacyCreatedTimeData()
        verify(mockCtrRepository, never()).deleteNStaleLegacyCreatedTimeData(anyInt())
      }
    }

    "call deleteNDataWithCreatedTimeStringType" when {
      "deleteNInvalidCreatedTimeData is enabled and limit is positive" in {
        when(mockCleanupConfig.deleteAllInvalidCreatedTimeData).thenReturn(false)
        when(mockCleanupConfig.deleteNInvalidCreatedTimeData).thenReturn(true)
        when(mockCleanupConfig.limitForDeletingInvalidCreatedTimeData).thenReturn(100)
        when(mockCtrRepository.deleteNStaleLegacyCreatedTimeData(100))
          .thenReturn(Future.successful(DeleteResult.acknowledged(50)))

        new TestStartUpJob().deleteInvalidData()

        verify(mockCtrRepository, times(1)).deleteNStaleLegacyCreatedTimeData(100)
        verify(mockCtrRepository, never()).deleteAllStaleLegacyCreatedTimeData()
      }
    }

    "call deleteAllDataWithCreatedTimeStringType" when {
      "deleteAllInvalidCreatedTimeData is enabled and deleteNInvalidCreatedTimeData is disabled" in {
        when(mockCleanupConfig.deleteAllInvalidCreatedTimeData).thenReturn(true)
        when(mockCleanupConfig.deleteNInvalidCreatedTimeData).thenReturn(false)
        when(mockCleanupConfig.limitForDeletingInvalidCreatedTimeData).thenReturn(1)
        when(mockCtrRepository.deleteNStaleLegacyCreatedTimeData(0))
          .thenReturn(Future.successful(DeleteResult.acknowledged(100)))

        new TestStartUpJob().deleteInvalidData()

        verify(mockCtrRepository, times(1)).deleteAllStaleLegacyCreatedTimeData()
        verify(mockCtrRepository, never()).deleteNStaleLegacyCreatedTimeData(anyInt())
      }
    }
  }
}