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

package services

import helpers.BaseSpec
import models._
import models.api.BusinessAddress
import org.mockito.ArgumentMatchers.{eq => eqTo}
import org.mockito.Mockito._
import play.api.test.Helpers._
import repositories.CorporationTaxRegistrationMongoRepository
import utils.LogCapturingHelper

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class GroupsServiceSpec extends BaseSpec with LogCapturingHelper {

  val regId = "testRegId"

  val validGroupsModel: Groups = Groups(
    groupRelief = true,
    nameOfCompany = Some(GroupCompanyName("testGroupName", GroupCompanyNameEnum.Other)),
    addressAndType = Some(GroupsAddressAndType(
      GroupAddressTypeEnum.ALF,
      BusinessAddress("1 abc", "2 abc", Some("3 abc"), Some("4 abc"), Some("ZZ1 1ZZ"), Some("country A")))
    ),
    groupUTR = Some(GroupUTR(Some("1234567890"))))


  class Setup {
    val service: GroupsService = new GroupsService {
      reset(mockCTDataRepository)
      override val cTRegistrationRepository: CorporationTaxRegistrationMongoRepository = mockCTDataRepository
      implicit val ec: ExecutionContext = global
    }
  }

  "returnGroups" must {
    "return groups if they exist in db" in new Setup {
      when(mockCTDataRepository.returnGroupsBlock(eqTo(regId))).thenReturn(Future.successful(Some(validGroupsModel)))
      val res: Option[Groups] = await(service.returnGroups(regId))
      res mustBe Some(validGroupsModel)
    }
    "return None if db returns nothing" in new Setup {
      when(mockCTDataRepository.returnGroupsBlock(eqTo(regId))).thenReturn(Future.successful(None))
      val res: Option[Groups] = await(service.returnGroups(regId))
      res mustBe None
    }

  }

  "deleteGroups" must {
    "return true if delete was successful" in new Setup {
      when(mockCTDataRepository.deleteGroupsBlock(eqTo(regId))).thenReturn(Future.successful(true))
      val res: Boolean = await(service.deleteGroups(regId))
      res mustBe true
    }
    "return future failed if delete was unsuccessful db returned an exception" in new Setup {
      when(mockCTDataRepository.deleteGroupsBlock(eqTo(regId))).thenReturn(Future.failed(new Exception("failure reason")))
      intercept[Exception](await(service.deleteGroups(regId)))
    }
  }

  "updateGroups" must {
    "return success of groups when db returns a success" in new Setup {
      when(mockCTDataRepository.updateGroups(eqTo(regId), eqTo(validGroupsModel))).thenReturn(Future.successful(validGroupsModel))
      val res: Groups = await(service.updateGroups(regId, validGroupsModel))
      res mustBe validGroupsModel
    }
    "return a future failed if db returns an exception" in new Setup {
      when(mockCTDataRepository.updateGroups(eqTo(regId), eqTo(validGroupsModel))).thenReturn(Future.failed(new Exception("failure reason")))
      intercept[Exception](await(service.updateGroups(regId, validGroupsModel)))
    }
  }
}