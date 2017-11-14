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

package services

import com.codahale.metrics.MetricRegistry
import mocks.MockMetricsService
import org.mockito.Matchers
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import uk.gov.hmrc.play.test.UnitSpec

class MetricsServiceSpec extends UnitSpec with MockitoSugar {

  val mockRegistry = mock[MetricRegistry]

  trait Setup {
    val service = MockMetricsService
  }

  "Metrics" should {
    "update no metrics if no registration stats" in new Setup() {
      when(service.ctRepository.getRegistrationStats()).thenReturn(Map[String, Int]())

      val result: Map[String, Int] = await(service.updateDocumentMetrics())

      result shouldBe Map()

      verifyNoMoreInteractions(mockRegistry)
    }

    "update a single metric when one is supplied" in new Setup() {
      when(service.metrics.defaultRegistry).thenReturn(mockRegistry)
      when(service.ctRepository.getRegistrationStats()).thenReturn(Map[String, Int]("test" -> 1))

      await(service.updateDocumentMetrics()) shouldBe Map("test" -> 1)

      verify(mockRegistry).remove(Matchers.any())
      verify(mockRegistry).register(Matchers.contains("test"), Matchers.any())
      verifyNoMoreInteractions(mockRegistry)
    }

    "update multiple metrics when required" in new Setup() {
      when(service.metrics.defaultRegistry).thenReturn(mockRegistry)
      when(service.ctRepository.getRegistrationStats()).thenReturn(Map[String, Int]("testOne" -> 1, "testTwo" -> 2, "testThree" -> 3))

      val result = await(service.updateDocumentMetrics())

      result shouldBe Map("testOne" -> 1, "testTwo" -> 2, "testThree" -> 3)

      verify(mockRegistry).remove(Matchers.contains("testOne"))
      verify(mockRegistry).register(Matchers.contains("testOne"), Matchers.any())
      verify(mockRegistry).remove(Matchers.contains("testTwo"))
      verify(mockRegistry).register(Matchers.contains("testTwo"), Matchers.any())
      verify(mockRegistry).remove(Matchers.contains("testThree"))
      verify(mockRegistry).register(Matchers.contains("testThree"), Matchers.any())
      verifyNoMoreInteractions(mockRegistry)
    }
  }
}
