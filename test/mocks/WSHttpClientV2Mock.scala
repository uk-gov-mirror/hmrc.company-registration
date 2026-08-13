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

package mocks

import org.mockito.ArgumentMatchers.{any, eq => eqTo}
import org.mockito.Mockito._
import org.mockito.stubbing.OngoingStubbing
import org.scalatestplus.mockito.MockitoSugar
import uk.gov.hmrc.http.client.{HttpClientV2, RequestBuilder}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, UpstreamErrorResponse}

import java.net.URL
import scala.concurrent.Future

trait WSHttpClientV2Mock {
  this: MockitoSugar =>

  lazy val mockHttpClientV2: HttpClientV2 = mock[HttpClientV2]
  lazy val requestBuilder: RequestBuilder = mock[RequestBuilder]

  def mockHttpPost[I](url: URL, payload: I, httpResponse: HttpResponse, mockHttp: HttpClientV2=mockHttpClientV2): OngoingStubbing[Future[HttpResponse]] = {
    when(mockHttp.post(eqTo(url))(using any[HeaderCarrier])).thenReturn(requestBuilder)
    when(requestBuilder.setHeader(any[Seq[(String, String)]]: _*)).thenReturn(requestBuilder)
    when(requestBuilder.withBody(eqTo(payload))(using any(), any(), any())).thenReturn(requestBuilder)
    when(requestBuilder.execute[HttpResponse](using any(), any())).thenReturn(Future.successful(httpResponse))
  }

  def mockHttpPostError[I](url: URL, payload: I, httpResponse: UpstreamErrorResponse, mockHttp: HttpClientV2=mockHttpClientV2): OngoingStubbing[Future[UpstreamErrorResponse]] = {
    when(mockHttp.post(eqTo(url))(using any[HeaderCarrier])).thenReturn(requestBuilder)
    when(requestBuilder.setHeader(any[Seq[(String, String)]]: _*)).thenReturn(requestBuilder)
    when(requestBuilder.withBody(eqTo(payload))(using any(), any(), any())).thenReturn(requestBuilder)
    when(requestBuilder.execute[UpstreamErrorResponse](using any(), any())).thenReturn(Future.failed(httpResponse))
  }

}
