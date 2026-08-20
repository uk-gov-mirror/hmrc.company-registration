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

import play.api.Configuration

import javax.inject.{Inject, Singleton}

@Singleton
class CreatedTimeDataCleanupConfig @Inject()(config: Configuration) {

  lazy val deleteAllInvalidCreatedTimeData: Boolean =
    config.getOptional[Boolean]("features.delete-all-invalid-createdTime-data").getOrElse(false)

  lazy val deleteNInvalidCreatedTimeData: Boolean =
    config.getOptional[Boolean]("features.delete-some-invalid-createdTime-data").getOrElse(false)

  lazy val limitForDeletingInvalidCreatedTimeData: Int =
    config.getOptional[Int]("features.limit-for-deleting-invalid-createdTime-data").getOrElse(1)

}