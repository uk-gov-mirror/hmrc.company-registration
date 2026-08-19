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

import org.scalatestplus.play.PlaySpec
import play.api.Configuration

class CreatedTimeDataCleanupConfigSpec extends PlaySpec {

  "CreatedTimeDataCleanupConfig" must {
    "read deleteAllInvalidCreatedTimeData from config" in {
      val config = Configuration(
        "features.delete-all-invalid-createdTime-data" -> true
      )
      val cleanupConfig = new CreatedTimeDataCleanupConfig(config)

      cleanupConfig.deleteAllInvalidCreatedTimeData mustBe true
    }

    "default deleteAllInvalidCreatedTimeData to false when not configured" in {
      val config = Configuration()
      val cleanupConfig = new CreatedTimeDataCleanupConfig(config)

      cleanupConfig.deleteAllInvalidCreatedTimeData mustBe false
    }

    "read deleteNInvalidCreatedTimeData from config" in {
      val config = Configuration(
        "features.delete-some-invalid-createdTime-data" -> true
      )
      val cleanupConfig = new CreatedTimeDataCleanupConfig(config)

      cleanupConfig.deleteNInvalidCreatedTimeData mustBe true
    }

    "default deleteNInvalidCreatedTimeData to false when not configured" in {
      val config = Configuration()
      val cleanupConfig = new CreatedTimeDataCleanupConfig(config)

      cleanupConfig.deleteNInvalidCreatedTimeData mustBe false
    }

    "read limitForDeletingInvalidCreatedTimeData from config" in {
      val config = Configuration(
        "features.limit-for-deleting-invalid-createdTime-data" -> 50
      )
      val cleanupConfig = new CreatedTimeDataCleanupConfig(config)

      cleanupConfig.limitForDeletingInvalidCreatedTimeData mustBe 50
    }

    "default limitForDeletingInvalidCreatedTimeData to 1 when not configured" in {
      val config = Configuration()
      val cleanupConfig = new CreatedTimeDataCleanupConfig(config)

      cleanupConfig.limitForDeletingInvalidCreatedTimeData mustBe 1
    }
  }
}
