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

package test.jobs

import com.google.inject.name.Names
import config.{LangConstants, MicroserviceAppConfig}
import jobs.{LockResponse, ScheduledJob}
import models._
import org.mongodb.scala.result.InsertOneResult
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.inject.{BindingKey, QualifierInstance}
import play.api.libs.json.{JsObject, Json, OWrites}
import play.api.test.Helpers._
import play.api.{Application, Logger}
import repositories.CorporationTaxRegistrationMongoRepository
import test.itutil.WiremockHelper.{stubDelete, stubGet, stubPost, verifyPOSTRequestBody}
import test.itutil.{IntegrationSpecBase, LogCapturingHelper, MongoIntegrationSpec, WiremockHelper}

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global

class RemoveStaleDocumentsJobISpec extends IntegrationSpecBase with MongoIntegrationSpec with LogCapturingHelper {

  lazy val appConfig = app.injector.instanceOf[MicroserviceAppConfig]

  val mockHost: String = WiremockHelper.wiremockHost
  val mockPort: Int = WiremockHelper.wiremockPort
  val mockUrl = s"http://$mockHost:$mockPort"

  val additionalConfiguration: Map[String, Any] = Map(
    "auditing.consumer.baseUri.host" -> s"$mockHost",
    "auditing.consumer.baseUri.port" -> s"$mockPort",
    "microservice.services.incorporation-information.host" -> s"$mockHost",
    "microservice.services.incorporation-information.port" -> s"$mockPort",
    "microservice.services.business-registration.host" -> s"$mockHost",
    "microservice.services.business-registration.port" -> s"$mockPort",
    "microservice.services.des-service.host" -> s"$mockHost",
    "microservice.services.des-service.port" -> s"$mockPort",
    "microservice.services.hip.host" -> s"$mockHost",
    "microservice.services.hip.port" -> s"$mockPort",
    "staleDocumentAmount" -> 4,
    "microservice.services.skipStaleDocs" -> "MSwyLDM=",
    "vat-threshold" -> List(
      Map(
      "dateTime" -> "2017-04-01T00:00:00",
      "amount" ->  85000),
      Map(
        "dateTime"-> "2024-03-31T23:00:00",
        "amount"-> 90000
      )
    )

  )
  override implicit lazy val app: Application = new GuiceApplicationBuilder()
    .configure(additionalConfiguration)
    .build()

  class Setup {
    val repository: CorporationTaxRegistrationMongoRepository = app.injector.instanceOf[CorporationTaxRegistrationMongoRepository]
    repository.deleteAll
    await(repository.ensureIndexes)
    repository.count mustBe 0


    implicit val jsObjWts: OWrites[JsObject] = OWrites(identity)

    def insert(reg: CorporationTaxRegistration): InsertOneResult = repository.insert(reg)

    def count: Int = repository.count
  }

  val txID = "txId"
  val txID2 = "txId2"
  val txID3 = "txId3"

  val regId = "regID"
  val regId2 = "regID2"
  val regId3 = "regID3"

  def corporationTaxRegistration(status: String = "draft",
                                 lastSignedIn: Instant = Instant.now(),
                                 regId: String = UUID.randomUUID().toString,
                                 txID: String = txID): CorporationTaxRegistration = CorporationTaxRegistration(
    status = status,
    internalId = "testID",
    registrationID = regId,
    formCreationTimestamp = "testDateTime",
    language = LangConstants.english,
    companyDetails = Some(CompanyDetails(
      "testCompanyName",
      CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
      PPOB("MANUAL", Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"), None, "txid"))),
      "testJurisdiction"
    )),
    contactDetails = Some(ContactDetails(
      Some("0123456789"), Some("0123456789"), Some("test@email.co.uk")
    )),
    tradingDetails = Some(TradingDetails("true")),
    confirmationReferences = Some(ConfirmationReferences("ackRef", txID, None, None)),
    acknowledgementReferences = None,
    createdTime = Instant.parse("2017-09-04T14:49:48.261Z"),
    lastSignedIn = lastSignedIn
  )


  def lookupJob(name: String): ScheduledJob = {
    val qualifier = Some(QualifierInstance(Names.named(name)))
    val key = BindingKey[ScheduledJob](classOf[ScheduledJob], qualifier)
    app.injector.instanceOf[ScheduledJob](key)
  }

  val topUpCtSubmissionUrl: String =
    if (appConfig.useHip) "/etmp/RESTAdapter/business-incorporation/CT" else "/business-incorporation/corporation-tax"

  "Remove Stale Documents Job" must {
    "delete 2 documents" when {
      "there are two stale document and 1 non-stale" in new Setup {
        stubGet(s"/incorporation-information/$txID/incorporation-update", 200, s"""{}""")
        stubGet(s"/incorporation-information/$txID2/incorporation-update", 200, s"""{}""")
        stubGet(s"/incorporation-information/$txID3/incorporation-update", 200, s"""{}""")

        stubGet(s"/business-registration/admin/business-tax-registration/remove/$regId2", 200, """{}""")
        stubGet(s"/business-registration/admin/business-tax-registration/remove/$regId3", 200, """{}""")

        stubDelete(s"/incorporation-information/subscribe/$txID3/regime/ctax/subscriber/SCRS?force=true", 200, s"""""")
        stubDelete(s"/incorporation-information/subscribe/$txID2/regime/ctax/subscriber/SCRS?force=true", 200, s"""""")

        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(84, ChronoUnit.DAYS), regId = regId, txID = txID))
        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(92, ChronoUnit.DAYS), regId = regId2, txID = txID2))
        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(93, ChronoUnit.DAYS), regId = regId3, txID = txID3))

        count mustBe 3
        val job: ScheduledJob = lookupJob("remove-stale-documents-job")
        val res: Either[Int, LockResponse] = await(job.scheduledMessage.service.invoke.map(_.asInstanceOf[Either[Int, LockResponse]]))

        res.left.getOrElse(-1) mustBe 2
        count mustBe 1
      }
    }

    "delete 1 documents" when {
      "there is one stale document" in new Setup {
        stubGet(s"/incorporation-information/$txID/incorporation-update", 200, s"""{}""")
        stubGet(s"/business-registration/admin/business-tax-registration/remove/$regId", 200, """{}""")
        stubDelete(s"/incorporation-information/subscribe/$txID/regime/ctax/subscriber/SCRS?force=true", 200, s"""""")

        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(93, ChronoUnit.DAYS), regId = regId))

        count mustBe 1

        val job: ScheduledJob = lookupJob("remove-stale-documents-job")
        val res: Either[Int, LockResponse] = await(job.scheduledMessage.service.invoke.map(_.asInstanceOf[Either[Int, LockResponse]]))

        res.left.getOrElse(-1) mustBe 1

        count mustBe 0
      }

      "there is one stale document using an old regime" in new Setup {
        stubGet(s"/incorporation-information/$txID/incorporation-update", 204, s"""{}""")
        stubGet(s"/business-registration/admin/business-tax-registration/remove/$regId", 200, """{}""")
        stubDelete(s"/incorporation-information/subscribe/$txID/regime/ctax/subscriber/SCRS?force=true", 404, s"""""")
        stubDelete(s"/incorporation-information/subscribe/$txID/regime/ct/subscriber/SCRS?force=true", 200, s"""""")

        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(93, ChronoUnit.DAYS), regId = regId))

        count mustBe 1

        val job: ScheduledJob = lookupJob("remove-stale-documents-job")
        val message = s"[AdminServiceImpl][processStaleDocument] Registration $regId - $txID does not have CTAX subscription. Now trying to delete CT sub."
        val delMess = "[AdminServiceImpl][invoke] Successfully deleted 1 stale documents"
        withCaptureOfLoggingFrom(Logger("application.services.admin.AdminServiceImpl")) { logs =>
          val res = await(job.scheduledMessage.service.invoke.map(_.asInstanceOf[Either[Int, LockResponse]]))
          res.left.getOrElse(-1) mustBe 1

          count mustBe 0

          logs.find(event => event.getMessage == message).get.getMessage mustBe message
          logs.find(event => event.getMessage == delMess).get.getMessage mustBe delMess
        }
      }

      "there is one stale document with no subscriptions" in new Setup {
        stubGet(s"/incorporation-information/$txID/incorporation-update", 204, s"""{}""")
        stubGet(s"/business-registration/admin/business-tax-registration/remove/$regId", 200, """{}""")
        stubDelete(s"/incorporation-information/subscribe/$txID/regime/ctax/subscriber/SCRS?force=true", 404, s"""""")
        stubDelete(s"/incorporation-information/subscribe/$txID/regime/ct/subscriber/SCRS?force=true", 404, s"""""")

        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(93, ChronoUnit.DAYS), regId = regId))

        count mustBe 1

        val job: ScheduledJob = lookupJob("remove-stale-documents-job")
        val message = s"[AdminServiceImpl][processStaleDocument] Registration $regId - $txID does not have CTAX subscription. Now trying to delete CT sub."
        val finalMessage = s"[AdminServiceImpl][processStaleDocument] Registration $regId - $txID has no subscriptions."
        val delMess = "[AdminServiceImpl][invoke] Successfully deleted 1 stale documents"

        withCaptureOfLoggingFrom(Logger("application.services.admin.AdminServiceImpl")) { logs =>
          val res = await(job.scheduledMessage.service.invoke.map(_.asInstanceOf[Either[Int, LockResponse]]))
          res.left.getOrElse(-1) mustBe 1

          count mustBe 0

          logs.find(event => event.getMessage.contains(message)).get.getMessage mustBe message
          logs.find(event => event.getMessage.contains(finalMessage)).get.getMessage mustBe finalMessage
          logs.find(event => event.getMessage.contains(delMess)).get.getMessage mustBe delMess
        }
      }

      "there is one stale document and 3 on the allow list" in new Setup {
        stubGet(s"/incorporation-information/$txID/incorporation-update", 200, s"""{}""")
        stubGet(s"/business-registration/admin/business-tax-registration/remove/$regId", 200, """{}""")
        stubDelete(s"/incorporation-information/subscribe/$txID/regime/ctax/subscriber/SCRS?force=true", 200, s"""""")

        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(93, ChronoUnit.DAYS), regId = regId))
        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(100, ChronoUnit.DAYS), regId = "2"))
        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(100, ChronoUnit.DAYS), regId = "3"))
        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(100, ChronoUnit.DAYS), regId = "1"))

        count mustBe 4

        val job: ScheduledJob = lookupJob("remove-stale-documents-job")
        val res: AnyVal = await(job.scheduledMessage.service.invoke.map(_.asInstanceOf[Either[Int, LockResponse]])).left.getOrElse(-1)

        res mustBe 1
        count mustBe 3
      }

      "there is one stale document and 2 non-stale" in new Setup {
        stubGet(s"/incorporation-information/$txID/incorporation-update", 200, s"""{}""")
        stubGet(s"/incorporation-information/$txID2/incorporation-update", 200, s"""{}""")
        stubGet(s"/incorporation-information/$txID3/incorporation-update", 200, s"""{}""")

        stubGet(s"/business-registration/admin/business-tax-registration/remove/$regId3", 200, """{}""")
        stubDelete(s"/incorporation-information/subscribe/$txID3/regime/ctax/subscriber/SCRS?force=true", 200, s"""""")

        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(84, ChronoUnit.DAYS), regId = regId, txID = txID))
        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(85, ChronoUnit.DAYS), regId = regId2, txID = txID2))
        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(93, ChronoUnit.DAYS), regId = regId3, txID = txID3))

        count mustBe 3

        val job: ScheduledJob = lookupJob("remove-stale-documents-job")
        val res: Int = await(job.scheduledMessage.service.invoke.map(_.asInstanceOf[Either[Int, LockResponse]])).left.getOrElse(-1)

        res mustBe 1
        count mustBe 2
      }

      "there is one stale document in held status" in new Setup {
        stubGet(s"/incorporation-information/$txID/incorporation-update", 200, s"""{}""")
        stubDelete(s"/incorporation-information/subscribe/txId/regime/ct/subscriber/SCRS?force=true", 200, "{}")
        stubGet(s"/business-registration/admin/business-tax-registration/remove/$regId", 200, """{}""")
        stubPost(topUpCtSubmissionUrl, 200, "{}")
        stubPost(s"/write/audit", 200, "{}")

        insert(corporationTaxRegistration(status = "held", lastSignedIn = Instant.now.minus(93, ChronoUnit.DAYS), regId = regId))

        count mustBe 1

        val job: ScheduledJob = lookupJob("remove-stale-documents-job")
        val res: Int = await(job.scheduledMessage.service.invoke.map(_.asInstanceOf[Either[Int, LockResponse]])).left.getOrElse(-1)

        res mustBe 1
        count mustBe 0

        val auditJson: JsObject = Json.obj("journeyId" -> regId, "acknowledgementReference" -> "ackRef", "incorporationStatus" -> "Rejected", "rejectedAsNotPaid" -> true)
        verifyPOSTRequestBody("/write/audit", auditJson.toString) mustBe true
      }
    }

    "delete 0 documents" when {
      "there is one stale document with a CRN" in new Setup {
        stubGet(s"/incorporation-information/$txID/incorporation-update", 200, s"""{"crn" : "crn"}""")
        stubGet(s"/business-registration/admin/business-tax-registration/remove/$regId", 200, """{}""")

        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(93, ChronoUnit.DAYS), regId = regId))

        count mustBe 1

        val job: ScheduledJob = lookupJob("remove-stale-documents-job")
        val res: Int = await(job.scheduledMessage.service.invoke.map(_.asInstanceOf[Either[Int, LockResponse]])).left.getOrElse(-1)

        res mustBe 0
        count mustBe 1
      }

      "there is one stale document of which it's BR document failed to delete" in new Setup {
        stubGet(s"/incorporation-information/$txID/incorporation-update", 200, s"""{}""")
        stubGet(s"/business-registration/admin/business-tax-registration/remove/$regId", 400, """{}""")

        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(93, ChronoUnit.DAYS), regId = regId))

        count mustBe 1

        val job: ScheduledJob = lookupJob("remove-stale-documents-job")
        val res: Int = await(job.scheduledMessage.service.invoke.map(_.asInstanceOf[Either[Int, LockResponse]])).left.getOrElse(-1)

        res mustBe 0

        count mustBe 1
      }

      "there are no stale documents" in new Setup {
        stubGet(s"/incorporation-information/$txID/incorporation-update", 200, s"""{}""")
        stubGet(s"/business-registration/admin/business-tax-registration/remove/$regId", 200, """{}""")

        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(85, ChronoUnit.DAYS), regId = regId))

        count mustBe 1

        val job: ScheduledJob = lookupJob("remove-stale-documents-job")
        val res: Int = await(job.scheduledMessage.service.invoke.map(_.asInstanceOf[Either[Int, LockResponse]])).left.getOrElse(-1)

        res mustBe 0

        count mustBe 1
      }

      "there is an old submitted document" in new Setup {
        stubGet(s"/incorporation-information/$txID/incorporation-update", 200, s"""{}""")
        stubGet(s"/business-registration/admin/business-tax-registration/remove/$regId", 200, """{}""")

        insert(corporationTaxRegistration(status = "submitted", lastSignedIn = Instant.now.minus(100, ChronoUnit.DAYS), regId = regId))

        count mustBe 1

        val job: ScheduledJob = lookupJob("remove-stale-documents-job")
        val res: Int = await(job.scheduledMessage.service.invoke.map(_.asInstanceOf[Either[Int, LockResponse]])).left.getOrElse(-1)

        res mustBe 0

        count mustBe 1
      }

      "the documents are on the allow list" in new Setup {
        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(100, ChronoUnit.DAYS), regId = "2"))
        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(100, ChronoUnit.DAYS), regId = "3"))
        insert(corporationTaxRegistration(lastSignedIn = Instant.now.minus(100, ChronoUnit.DAYS), regId = "1"))

        count mustBe 3

        val job: ScheduledJob = lookupJob("remove-stale-documents-job")
        val res: Int = await(job.scheduledMessage.service.invoke.map(_.asInstanceOf[Either[Int, LockResponse]])).left.getOrElse(-1)

        res mustBe 0

        count mustBe 3
      }
    }
  }

}
