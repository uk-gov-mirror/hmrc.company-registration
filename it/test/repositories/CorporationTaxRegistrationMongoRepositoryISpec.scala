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

package test.repositories

import auth.CryptoSCRS
import config.LangConstants
import models.RegistrationStatus.{LOCKED, _}
import models._
import models.api.BusinessAddress
import models.validation.MongoValidation
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Projections
import org.mongodb.scala.result.{InsertOneResult, UpdateResult}
import org.scalatest.Assertion
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json._
import play.api.test.Helpers._
import repositories.{CorporationTaxRegistrationMongoRepository, MissingCTDocument}
import test.fixtures.CorporationTaxRegistrationFixture
import test.fixtures.CorporationTaxRegistrationFixture.ctRegistrationJson
import test.itutil.ItTestConstants.CorporationTaxRegistration.corpTaxRegModel
import test.itutil.ItTestConstants.TakeoverDetails.{testTakeoverDetails, testTakeoverDetailsModel}
import test.itutil.{IntegrationSpecBase, MongoIntegrationSpec}
import org.bson.{BsonType, Document}

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CorporationTaxRegistrationMongoRepositoryISpec
  extends IntegrationSpecBase with MongoIntegrationSpec {

  val additionalConfiguration: Map[String, String] = Map(
    "schedules.missing-incorporation-job.enabled" -> "false",
    "schedules.metrics-job.enabled" -> "false",
    "schedules.remove-stale-documents-job.enabled" -> "false"
  )
  override implicit lazy val app: Application = new GuiceApplicationBuilder()
    .configure(additionalConfiguration)
    .build()

  def now: Instant = LocalDateTime.now().withNano(0).toInstant(ZoneOffset.UTC)

  class Setup {
    val repository: CorporationTaxRegistrationMongoRepository = app.injector.instanceOf[CorporationTaxRegistrationMongoRepository]
    repository.deleteAll()
    await(repository.ensureIndexes)

    implicit val jsObjWts: OWrites[JsObject] = OWrites(identity)

    def insert(reg: CorporationTaxRegistration): Assertion = {
      val currentCount = count
      repository.insert(reg)
      count mustBe currentCount + 1
    }

    def insertMultiple(reg: Seq[CorporationTaxRegistration]): Assertion = {
      val currentCount = count
      repository.insertMany(reg)
      count mustBe currentCount + reg.length
    }

    def count: Int = repository.count

    def retrieve(regId: String): Option[CorporationTaxRegistration] = await(repository.findOneBySelector(repository.regIDSelector(regId)))
  }

  val validGroupsModel: Groups = Groups(
    groupRelief = true,
    nameOfCompany = Some(GroupCompanyName("testGroupName", GroupCompanyNameEnum.Other)),
    addressAndType = Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF, BusinessAddress("1 abc", "2 abc", Some("3 abc"), Some("4 abc"), Some("ZZ1 1ZZ"), Some("country A")))),
    groupUTR = Some(GroupUTR(Some("1234567890"))))

  def setupCollection(repo: CorporationTaxRegistrationMongoRepository, ctRegistration: CorporationTaxRegistration): InsertOneResult = {
    repo.insert(ctRegistration)
  }

  val ackRef = "test-ack-ref"

  def corporationTaxRegistration(status: String = "draft",
                                 ctutr: Boolean = true,
                                 lastSignedIn: Instant = now,
                                 registrationStatus: String = RegistrationStatus.DRAFT,
                                 regId: String = UUID.randomUUID().toString): CorporationTaxRegistration = CorporationTaxRegistration(
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
    confirmationReferences = Some(ConfirmationReferences(acknowledgementReference = ackRef, "txId", None, None)),
    acknowledgementReferences = Some(AcknowledgementReferences(Option("ctutr").filter(_ => ctutr), "timestamp", status)),
    createdTime = Instant.parse("2017-09-04T14:49:48.261Z"),
    lastSignedIn = lastSignedIn
  )

  val registrationId = "testRegId"

  def newCTDoc: CorporationTaxRegistration = CorporationTaxRegistration(
    internalId = "testID",
    registrationID = registrationId,
    formCreationTimestamp = "testDateTime",
    language = LangConstants.english
  )

  val testRoAddress: CHROAddress = CHROAddress("premises", "address line 1", None, "uk", "local", None, None, None)

  "retrieveCorporationTaxRegistration" must {
    "retrieve a registration with an invalid phone number" in new Setup {
      val registrationId = "testRegId"

      val corporationTaxRegistration: CorporationTaxRegistration = CorporationTaxRegistration(
        internalId = "testID",
        registrationID = registrationId,
        formCreationTimestamp = "testDateTime",
        language = LangConstants.english,
        contactDetails = Some(ContactDetails(
          phone = Some("12345"),
          mobile = Some("1234567890"),
          email = None))
      )

      setupCollection(repository, corporationTaxRegistration)
      val response: Future[Option[CorporationTaxRegistration]] = repository.findOneBySelector(repository.regIDSelector(registrationId))
      await(response).flatMap(_.contactDetails) mustBe corporationTaxRegistration.contactDetails
    }
    "retrieve ctdoc with new groups block" in new Setup {
      val ctDoc: CorporationTaxRegistration = corporationTaxRegistration(regId = "123").copy(groups = Some(validGroupsModel))
      val resOfInsert: CorporationTaxRegistration = await(repository.createCorporationTaxRegistration(ctDoc))
      count mustBe 1
      val response: Option[CorporationTaxRegistration] = await(repository.findOneBySelector(repository.regIDSelector(resOfInsert.registrationID)))
      response.get.groups.get mustBe validGroupsModel
    }
  }

  "getExistingRegistration" must {
    "throw an exception if no document is found" in new Setup {
      intercept[MissingCTDocument](await(repository.getExistingRegistration(registrationId)))
    }

    "retrieve a document if exists" in new Setup {
      val corporationTaxRegistration: CorporationTaxRegistration = CorporationTaxRegistration(
        internalId = "testID",
        registrationID = registrationId,
        formCreationTimestamp = "testDateTime",
        language = LangConstants.english,
        contactDetails = Some(ContactDetails(
          phone = Some("12345"),
          mobile = Some("1234567890"),
          email = None))
      )

      insert(corporationTaxRegistration)
      val response: CorporationTaxRegistration = await(repository.getExistingRegistration(registrationId))
      response.copy(createdTime = now, lastSignedIn = now) mustBe corporationTaxRegistration.copy(createdTime = now, lastSignedIn = now)
    }
  }

  "updateAccountingDetails" must {
    "return some updated company details if document exists" in new Setup {

      val accountingDetails: AccountingDetails = AccountingDetails("some-status", Some("some-date"))

      insert(newCTDoc)
      val response: Option[AccountingDetails] = await(repository.updateAccountingDetails(registrationId, accountingDetails))
      response mustBe Some(accountingDetails)
    }

    "return None if no document exists" in new Setup {
      val accountingDetails: AccountingDetails = AccountingDetails("some-status", Some("some-date"))

      val response: Option[AccountingDetails] = await(repository.updateAccountingDetails(registrationId, accountingDetails))
      response mustBe None
    }
  }

  "retrieveAccountingDetails" must {
    "return some company details if document exists" in new Setup {
      val accountingDetails: AccountingDetails = AccountingDetails("some-status", Some("some-date"))

      val corporationTaxRegistrationModel: CorporationTaxRegistration = newCTDoc.copy(
        accountingDetails = Some(accountingDetails)
      )

      insert(corporationTaxRegistrationModel)
      val response: Option[AccountingDetails] = await(repository.retrieveAccountingDetails(registrationId))
      response mustBe Some(accountingDetails)
    }

    "return None if no accounting details are present in the ct doc" in new Setup {
      insert(newCTDoc)
      val response: Option[AccountingDetails] = await(repository.retrieveAccountingDetails(registrationId))
      response mustBe None
    }

    "return None if no document exists" in new Setup {
      val response: Option[AccountingDetails] = await(repository.retrieveAccountingDetails(registrationId))
      response mustBe None
    }
  }

  "updateCompanyDetails" must {
    "return some updated company details if document exists" in new Setup {
      val companyDetails: CompanyDetails = CompanyDetails("company-name", testRoAddress, PPOB("RO", None), "jurisdiction")

      insert(newCTDoc)
      val response: Option[CompanyDetails] = await(repository.updateCompanyDetails(registrationId, companyDetails))
      response mustBe Some(companyDetails)
    }

    "return None if no document exists" in new Setup {
      val companyDetails: CompanyDetails = CompanyDetails("company-name", testRoAddress, PPOB("RO", None), "jurisdiction")

      val response: Option[CompanyDetails] = await(repository.updateCompanyDetails(registrationId, companyDetails))
      response mustBe None
    }
  }

  "retrieveCompanyDetails" must {
    "return some company details if document exists" in new Setup {
      val registrationId = "testRegId"

      val companyDetails: CompanyDetails = CompanyDetails("company-name", testRoAddress, PPOB("RO", None), "jurisdiction")

      val corporationTaxRegistrationModel: CorporationTaxRegistration = newCTDoc.copy(
        companyDetails = Some(companyDetails)
      )

      insert(corporationTaxRegistrationModel)
      val response: Option[CompanyDetails] = await(repository.retrieveCompanyDetails(registrationId))
      response mustBe Some(companyDetails)
    }

    "return None if no company details are present in the ct doc" in new Setup {
      insert(newCTDoc)
      val response: Option[CompanyDetails] = await(repository.retrieveCompanyDetails(registrationId))
      response mustBe None
    }

    "return None if no document exists" in new Setup {
      val response: Option[CompanyDetails] = await(repository.retrieveCompanyDetails(registrationId))
      response mustBe None
    }
  }

  "updateTradingDetails" must {
    "return some updated trading details if document exists" in new Setup {
      val tradingDetails: TradingDetails = TradingDetails("yes")

      insert(newCTDoc)
      val response: Option[TradingDetails] = await(repository.updateTradingDetails(registrationId, tradingDetails))
      response mustBe Some(tradingDetails)
    }

    "return override trading details if document already has trading details" in new Setup {
      val tradingDetails: TradingDetails = TradingDetails("yes")
      val newTradingDetails: TradingDetails = TradingDetails()

      insert(newCTDoc.copy(tradingDetails = Some(tradingDetails)))
      val response: Option[TradingDetails] = await(repository.updateTradingDetails(registrationId, newTradingDetails))
      response mustBe Some(newTradingDetails)
    }

    "return None if no document exists" in new Setup {
      val tradingDetails: TradingDetails = TradingDetails("yes")

      val response: Option[TradingDetails] = await(repository.updateTradingDetails(registrationId, tradingDetails))
      response mustBe None
    }
  }

  "retrieveTradingDetails" must {
    "return some trading details if document exists" in new Setup {
      val registrationId = "testRegId"

      val tradingDetails: TradingDetails = TradingDetails("yes")

      val corporationTaxRegistrationModel: CorporationTaxRegistration = newCTDoc.copy(
        tradingDetails = Some(tradingDetails)
      )

      insert(corporationTaxRegistrationModel)
      val response: Option[TradingDetails] = await(repository.retrieveTradingDetails(registrationId))
      response mustBe Some(tradingDetails)
    }

    "return None if no trading details are present in the ct doc" in new Setup {
      insert(newCTDoc)
      await(repository.retrieveTradingDetails(registrationId)) mustBe None
    }

    "return None if no document exists" in new Setup {
      await(repository.retrieveTradingDetails(registrationId)) mustBe None
    }
  }

  "updateContactDetails" must {
    "return some updated contact details if document exists" in new Setup {
      val testContactDetails: ContactDetails = ContactDetails(
        phone = None,
        email = None,
        mobile = None
      )

      insert(newCTDoc)
      await(repository.updateContactDetails(registrationId, testContactDetails)) mustBe Some(testContactDetails)
    }

    "return override contact details if document already has contact details" in new Setup {
      val contactDetails: ContactDetails = ContactDetails(
        phone = None,
        email = None,
        mobile = None
      )
      val newContactDetails: ContactDetails = contactDetails.copy(phone = Some("12333334234234"))

      insert(newCTDoc.copy(contactDetails = Some(contactDetails)))
      await(repository.updateContactDetails(registrationId, newContactDetails)) mustBe Some(newContactDetails)
    }

    "return None if no document exists" in new Setup {
      val contactDetails: ContactDetails = ContactDetails(
        phone = None,
        email = None,
        mobile = None
      )

      await(repository.updateContactDetails(registrationId, contactDetails)) mustBe None
    }
  }

  "retrieveContactDetails" must {
    "return none when there is no document" in new Setup {
      await(repository.retrieveContactDetails(registrationId)) mustBe None
    }
    "return none when there are no contact details" in new Setup {
      insert(newCTDoc)
      await(repository.retrieveContactDetails(registrationId)) mustBe None
    }
    "return some when there are contact details" in new Setup {
      val testContactDetails: ContactDetails = ContactDetails(
        phone = Some("123456"),
        email = None,
        mobile = None
      )
      insert(newCTDoc.copy(contactDetails = Some(testContactDetails)))
      await(repository.retrieveContactDetails(registrationId)) mustBe Some(testContactDetails)
    }
  }

  "updateConfirmationReferences" must {
    "return some confirmation references if document exists" in new Setup {
      val confirmationReferences: ConfirmationReferences = ConfirmationReferences(
        acknowledgementReference = "some-ackref",
        transactionId = "some-txid",
        paymentReference = None,
        paymentAmount = None
      )

      insert(newCTDoc)
      await(repository.updateConfirmationReferences(registrationId, confirmationReferences)) mustBe Some(confirmationReferences)
    }

    "return override confirmation references if document already has confirmation references" in new Setup {
      val confirmationReferences: ConfirmationReferences = ConfirmationReferences(
        acknowledgementReference = "some-ackref",
        transactionId = "some-txid",
        paymentReference = None,
        paymentAmount = None
      )
      val newConfirmationReferences: ConfirmationReferences = confirmationReferences.copy(paymentReference = Some("Some-pay-ref"), paymentAmount = Some("12333334234234"))

      insert(newCTDoc.copy(confirmationReferences = Some(confirmationReferences)))
      await(repository.updateConfirmationReferences(registrationId, newConfirmationReferences)) mustBe Some(newConfirmationReferences)
    }

    "return None if no document exists" in new Setup {
      val confirmationReferences: ConfirmationReferences = ConfirmationReferences(
        acknowledgementReference = "some-ackref",
        transactionId = "some-txid",
        paymentReference = None,
        paymentAmount = None
      )

      await(repository.updateConfirmationReferences(registrationId, confirmationReferences)) mustBe None
    }
  }

  "retrieveConfirmationReferences" must {
    "return none when there is no document" in new Setup {
      await(repository.retrieveConfirmationReferences(registrationId)) mustBe None
    }
    "return none when there are no confirmation references" in new Setup {
      insert(newCTDoc)
      await(repository.retrieveConfirmationReferences(registrationId)) mustBe None
    }
    "return some when there are confirmation references" in new Setup {
      val confirmationReferences: ConfirmationReferences = ConfirmationReferences(
        acknowledgementReference = "some-ackref",
        transactionId = "some-txid",
        paymentReference = None,
        paymentAmount = None
      )

      insert(newCTDoc.copy(confirmationReferences = Some(confirmationReferences)))
      await(repository.retrieveConfirmationReferences(registrationId)) mustBe Some(confirmationReferences)
    }
  }

  "updateConfirmationReferencesAndStatus" must {
    "return some confirmation references if document exists" in new Setup {
      val confirmationReferences: ConfirmationReferences = ConfirmationReferences(
        acknowledgementReference = "some-ackref",
        transactionId = "some-txid",
        paymentReference = None,
        paymentAmount = None
      )

      insert(newCTDoc)
      await(repository.updateConfirmationReferencesAndUpdateStatus(registrationId, confirmationReferences, "passed")) mustBe Some(confirmationReferences)
      retrieve(registrationId).get.status mustBe "passed"
    }

    "return override confirmation references if document already has confirmation references and status" in new Setup {
      val confirmationReferences: ConfirmationReferences = ConfirmationReferences(
        acknowledgementReference = "some-ackref",
        transactionId = "some-txid",
        paymentReference = None,
        paymentAmount = None
      )
      val newConfirmationReferences: ConfirmationReferences = confirmationReferences.copy(paymentReference = Some("Some-pay-ref"), paymentAmount = Some("12333334234234"))

      insert(newCTDoc.copy(confirmationReferences = Some(confirmationReferences), status = "passed"))
      await(repository.updateConfirmationReferencesAndUpdateStatus(registrationId, newConfirmationReferences, "unpassed")) mustBe Some(newConfirmationReferences)
      retrieve(registrationId).get.status mustBe "unpassed"
    }

    "return None if no document exists" in new Setup {
      val confirmationReferences: ConfirmationReferences = ConfirmationReferences(
        acknowledgementReference = "some-ackref",
        transactionId = "some-txid",
        paymentReference = None,
        paymentAmount = None
      )

      await(repository.updateConfirmationReferencesAndUpdateStatus(registrationId, confirmationReferences, "passed")) mustBe None
    }
  }

  "updateCompanyEndDate" must {
    "return some Accounting Details prep if document exists" in new Setup {
      val accountingPrepDetails: AccountPrepDetails = AccountPrepDetails(
        status = "end",
        endDate = None
      )

      insert(newCTDoc)
      await(repository.updateCompanyEndDate(registrationId, accountingPrepDetails)) mustBe Some(accountingPrepDetails)
    }

    "return override accounting Details prep if document already has Accounting Details prep" in new Setup {
      val accountingPrepDetails: AccountPrepDetails = AccountPrepDetails(
        status = "end",
        endDate = None
      )
      val newAccountingPrepDetails: AccountPrepDetails = accountingPrepDetails.copy(endDate = Some(LocalDate.now()))

      insert(newCTDoc.copy(accountsPreparation = Some(accountingPrepDetails)))
      await(repository.updateCompanyEndDate(registrationId, newAccountingPrepDetails)) mustBe Some(newAccountingPrepDetails)
    }

    "return None if no document exists" in new Setup {
      val accountingPrepDetails: AccountPrepDetails = AccountPrepDetails(
        status = "end",
        endDate = None
      )

      await(repository.updateCompanyEndDate(registrationId, accountingPrepDetails)) mustBe None
    }
  }

  "updateEmail" must {
    "return some email if document exists" in new Setup {
      val email: Email = Email(
        address = "end@end.end",
        emailType = "type",
        linkSent = false,
        verified = false,
        returnLinkEmailSent = false
      )

      insert(newCTDoc)
      await(repository.updateEmail(registrationId, email)) mustBe Some(email)
    }

    "return override email if document already has email" in new Setup {
      val email: Email = Email(
        address = "end@end.end",
        emailType = "type",
        linkSent = false,
        verified = false,
        returnLinkEmailSent = false
      )
      val newEmail: Email = email.copy(linkSent = true, returnLinkEmailSent = true)

      insert(newCTDoc.copy(verifiedEmail = Some(email)))
      await(repository.updateEmail(registrationId, newEmail)) mustBe Some(newEmail)
    }

    "return None if no document exists" in new Setup {
      val email: Email = Email(
        address = "end@end.end",
        emailType = "type",
        linkSent = false,
        verified = false,
        returnLinkEmailSent = false
      )

      await(repository.updateEmail(registrationId, email)) mustBe None
    }
  }

  "retrieveEmail" must {
    "return none when there is no document" in new Setup {
      await(repository.retrieveEmail(registrationId)) mustBe None
    }
    "return none when there is no email block" in new Setup {
      insert(newCTDoc)
      await(repository.retrieveEmail(registrationId)) mustBe None
    }

    "return some when there is an email block" in new Setup {
      val email: Email = Email(
        address = "some@address.com",
        emailType = "type",
        linkSent = false,
        verified = false,
        returnLinkEmailSent = false
      )

      insert(newCTDoc.copy(verifiedEmail = Some(email)))
      await(repository.retrieveEmail(registrationId)) mustBe Some(email)
    }
  }

  "updateLanguage" must {
    "update the stored language" in new Setup {

      insert(newCTDoc)
      retrieve(registrationId).map(_.language) mustBe Some(LangConstants.english)

      await(repository.updateLanguage(registrationId, Language(LangConstants.welsh))) mustBe Some(Language(LangConstants.welsh))

      retrieve(registrationId).map(_.language) mustBe Some(LangConstants.welsh)
    }

    "return None if no document exists" in new Setup {
      retrieve(registrationId).map(_.language) mustBe None
    }
  }

  "retrieveLanguage" must {
    "return none when there is no document" in new Setup {
      await(repository.retrieveLanguage(registrationId)) mustBe None
    }

    "return Some(Language) when there is a document" in new Setup {
      insert(newCTDoc)
      await(repository.retrieveLanguage(registrationId)) mustBe Some(Language(LangConstants.english))
    }
  }

  "updateRegistrationProgress" must {
    "return some registrationPrgoress if document exists" in new Setup {
      val registrationPrgoress: String = "progress"
      insert(newCTDoc)
      await(repository.updateRegistrationProgress(registrationId, registrationPrgoress)) mustBe Some(registrationPrgoress)
    }

    "return override registration progress if document already has registration progress" in new Setup {
      val registrationProgress: String = "progress"
      val newRegistrationProgress: String = "stopped"

      insert(newCTDoc.copy(registrationProgress = Some(registrationProgress)))
      await(repository.updateRegistrationProgress(registrationId, newRegistrationProgress)) mustBe Some(newRegistrationProgress)
    }

    "return None if no document exists" in new Setup {
      val registrationProgress: String = "progress"

      await(repository.updateRegistrationProgress(registrationId, registrationProgress)) mustBe None
    }
  }
  "removeUnnecessaryRegistrationInformation" must {
    "clear all un-needed data" when {
      "mongo statement executes with no errors" in new Setup {
        val registrationId = "testRegId"

        val corporationTaxRegistrationModel: CorporationTaxRegistration = CorporationTaxRegistration(
          internalId = "testID",
          registrationID = registrationId,
          formCreationTimestamp = "testDateTime",
          language = LangConstants.english
        )


        setupCollection(repository, corporationTaxRegistrationModel)
        await(repository.removeUnnecessaryRegistrationInformation(registrationId)) mustBe true

        val corporationTaxRegistration: CorporationTaxRegistration = await(repository.findOneBySelector(repository.regIDSelector(registrationId))).get
        corporationTaxRegistration.verifiedEmail.isEmpty mustBe true
        corporationTaxRegistration.companyDetails.isEmpty mustBe true
        corporationTaxRegistration.accountingDetails.isEmpty mustBe true
        corporationTaxRegistration.registrationID mustBe corporationTaxRegistrationModel.registrationID
        corporationTaxRegistration.status mustBe corporationTaxRegistrationModel.status
        corporationTaxRegistration.lastSignedIn.toEpochMilli mustBe corporationTaxRegistrationModel.lastSignedIn.toEpochMilli
      }

      "mongo statement execeutes with the document having missing fields" in new Setup {
        val registrationId = "testRegId"

        val corporationTaxRegistrationModel: CorporationTaxRegistration = CorporationTaxRegistration(
          internalId = "testID",
          registrationID = registrationId,
          formCreationTimestamp = "testDateTime",
          language = LangConstants.english,
          companyDetails = None
        )

        setupCollection(repository, corporationTaxRegistrationModel)
        await(repository.removeUnnecessaryRegistrationInformation(registrationId)) mustBe true

        val corporationTaxRegistration: CorporationTaxRegistration = await(repository.findOneBySelector(repository.regIDSelector(registrationId))).get
        corporationTaxRegistration.verifiedEmail.isEmpty mustBe true
        corporationTaxRegistration.companyDetails.isEmpty mustBe true
        corporationTaxRegistration.accountingDetails.isEmpty mustBe true
        corporationTaxRegistration.registrationID mustBe corporationTaxRegistrationModel.registrationID
        corporationTaxRegistration.status mustBe corporationTaxRegistrationModel.status
        corporationTaxRegistration.lastSignedIn.toEpochMilli mustBe corporationTaxRegistrationModel.lastSignedIn.toEpochMilli
      }
    }
    "should not update document" when {
      "when the document does not exist" in new Setup {
        val registrationId = "testRegId"
        val incorrectRegistrationId = "notTheTestRegId"

        val corporationTaxRegistrationModel: CorporationTaxRegistration = CorporationTaxRegistration(
          internalId = "testID",
          registrationID = registrationId,
          formCreationTimestamp = "testDateTime",
          language = LangConstants.english,
          companyDetails = None
        )

        setupCollection(repository, corporationTaxRegistrationModel)
        await(repository.removeUnnecessaryRegistrationInformation(incorrectRegistrationId)) mustBe true
      }
    }
  }


  "updateSubmissionStatus" must {

    val registrationId = "testRegId"

    val corporationTaxRegistration = CorporationTaxRegistration(
      internalId = "testID",
      registrationID = registrationId,
      formCreationTimestamp = "testDateTime",
      language = LangConstants.english
    )

    "return an updated held status" in new Setup {
      val status = "held"

      insert(corporationTaxRegistration)

      val response: Future[String] = repository.updateSubmissionStatus(registrationId, status)
      await(response) mustBe "held"
    }
    "return an exception when document is missing because mapping on result will not contain status" in new Setup {
      intercept[MissingCTDocument](await(repository.updateSubmissionStatus(registrationId, "testStatus")))
    }
  }

  "removeTaxRegistrationInformation" must {

    val registrationId = "testRegId"

    val corporationTaxRegistration = CorporationTaxRegistration(
      internalId = "testID",
      registrationID = registrationId,
      formCreationTimestamp = "testDateTime",
      language = LangConstants.english,
      companyDetails = Some(CompanyDetails(
        "testCompanyName",
        CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
        PPOB("MANUAL", Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"), None, "txid"))),
        "testJurisdiction"
      )),
      contactDetails = Some(ContactDetails(Some("0123456789"), Some("0123456789"), Some("test@email.co.uk")
      )),
      tradingDetails = Some(TradingDetails("true"))
    )

    "remove companyDetails, contactDetails and tradingDetails objects from the collection" in new Setup {
      setupCollection(repository, corporationTaxRegistration)

      val response: Future[Boolean] = repository.removeTaxRegistrationInformation(registrationId)
      await(response) mustBe true
    }
    "remove just company details when contact details and trading details are already empty" in new Setup {
      setupCollection(repository, corporationTaxRegistration.copy(tradingDetails = None, contactDetails = None))

      val response: Future[Boolean] = repository.removeTaxRegistrationInformation(registrationId)
      await(response) mustBe true
    }
    "throw a MissingCTDocument exception when document does not exist" in new Setup {
      intercept[MissingCTDocument](await(repository.removeTaxRegistrationInformation("testRegId")))
    }
  }
  "getInternalId" must {
    "return None when no CT Doc exists" in new Setup {
      intercept[MissingCTDocument](await(repository.getInternalId("testInternalId")))
    }
    "return Some(regId, InternalId) when ct doc exists" in new Setup {
      insert(newCTDoc)
      await(repository.getInternalId(registrationId)) mustBe (newCTDoc.registrationID -> newCTDoc.internalId)
    }
  }

  "updateCTRecordWithAcknowledgments" must {

    val ackRef = "BRCT12345678910"

    val validConfirmationReferences = ConfirmationReferences(
      acknowledgementReference = "BRCT12345678910",
      transactionId = "TX1",
      paymentReference = Some("PY1"),
      paymentAmount = Some("12.00")
    )

    val validAckRefs = AcknowledgementReferences(
      ctUtr = Option("CTUTR123456789"),
      timestamp = "856412556487",
      status = "success"
    )

    val validHeldCorporationTaxRegistration = CorporationTaxRegistration(
      internalId = "9876543210",
      registrationID = "0123456789",
      status = HELD,
      formCreationTimestamp = "2001-12-31T12:00:00Z",
      language = LangConstants.english,
      acknowledgementReferences = Some(validAckRefs),
      confirmationReferences = Some(validConfirmationReferences),
      companyDetails = None,
      accountingDetails = None,
      tradingDetails = None,
      contactDetails = None
    )

    "return a WriteResult with no errors" in new Setup {
      val result: UpdateResult = await(repository.updateCTRecordWithAcknowledgments(ackRef, validHeldCorporationTaxRegistration))
      result.getModifiedCount mustBe 0
    }
  }

  "retrieveByAckRef" must {

    val ackRef = "BRCT12345678910"

    val validConfirmationReferences = ConfirmationReferences(ackRef, "TX1", Some("PY1"), Some("12.00"))

    val validHeldCorporationTaxRegistration = CorporationTaxRegistration(
      internalId = "9876543210",
      registrationID = "0123456789",
      status = HELD,
      formCreationTimestamp = "2001-12-31T12:00:00Z",
      language = LangConstants.english,
      confirmationReferences = Some(validConfirmationReferences),
      createdTime = now,
      lastSignedIn = now
    )

    "return an optional ct record" when {

      "given an ack ref" in new Setup {
        setupCollection(repository, validHeldCorporationTaxRegistration)

        val result: CorporationTaxRegistration = await(repository.findOneBySelector(repository.ackRefSelector(ackRef))).get
        result mustBe validHeldCorporationTaxRegistration
      }
    }
  }

  "Update registration to submitted (with data)" must {

    val ackRef = "BRCT12345678910"

    val heldReg = CorporationTaxRegistration(
      internalId = "9876543210",
      registrationID = "0123456789",
      status = HELD,
      formCreationTimestamp = "2001-12-31T12:00:00Z",
      language = LangConstants.english,
      confirmationReferences = Some(ConfirmationReferences(ackRef, "TX1", Some("PY1"), Some("12.00"))),
      accountingDetails = Some(AccountingDetails(AccountingDetails.WHEN_REGISTERED, None)),
      accountsPreparation = Some(AccountPrepDetails(AccountPrepDetails.HMRC_DEFINED, None))
    )

    "update the CRN and timestamp" in new Setup {
      setupCollection(repository, heldReg)

      val crn = "crn1234"
      val submissionTS = "2001-12-31T12:00:00Z"

      val result: Boolean = await(repository.updateHeldToSubmitted(heldReg.registrationID, crn, submissionTS))

      result mustBe true

      val someActual: Option[CorporationTaxRegistration] = await(repository.findOneBySelector(repository.regIDSelector(heldReg.registrationID)))
      someActual mustBe defined
      val actual: CorporationTaxRegistration = someActual.get
      actual.status mustBe RegistrationStatus.SUBMITTED
      actual.crn mustBe Some(crn)
      actual.submissionTimestamp mustBe Some(submissionTS)
      actual.accountingDetails mustBe None
      actual.accountsPreparation mustBe None
    }

    "fail to update the CRN" in new Setup {

      setupCollection(repository, heldReg.copy(registrationID = "ABC"))

      val crn = "crn1234"
      val submissionTS = "2001-12-31T12:00:00Z"

      intercept[MissingCTDocument] {
        await(repository.updateHeldToSubmitted(heldReg.registrationID, crn, submissionTS))
      }

    }

  }

  "removeTaxRegistrationById" must {
    val regId = UUID.randomUUID.toString

    val corporationTaxRegistration = CorporationTaxRegistration(
      internalId = "intId",
      registrationID = regId,
      formCreationTimestamp = "testDateTime",
      language = LangConstants.english,
      companyDetails = Some(CompanyDetails(
        "testCompanyName",
        CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
        PPOB("LOOKUP", Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"), Some("xxx"), "txid"))),
        "testJurisdiction"
      )),
      contactDetails = Some(ContactDetails(Some("0123456789"), Some("0123456789"), Some("test@email.co.uk")
      )),
      tradingDetails = Some(TradingDetails("true")),
      createdTime = now,
      lastSignedIn = now
    )

    "remove all details under that RegId from the collection" in new Setup {
      setupCollection(repository, corporationTaxRegistration)

      lazy val response: Future[Boolean] = repository.removeTaxRegistrationById(regId)

      retrieve(regId) mustBe Some(corporationTaxRegistration)
      await(response) mustBe true
      retrieve(regId) mustBe None
    }

    "remove only the data associated with the supplied regId" in new Setup {
      count mustBe 0
      insert(corporationTaxRegistration)
      insert(corporationTaxRegistration.copy(registrationID = "otherRegId"))
      count mustBe 2

      val result: Boolean = await(repository.removeTaxRegistrationById(regId))

      count mustBe 1
      retrieve(regId) mustBe None
      retrieve("otherRegId") mustBe Some(corporationTaxRegistration.copy(registrationID = "otherRegId"))
    }

    "experience an missing ct doc exception if not document exists" in new Setup {
      intercept[MissingCTDocument](await(repository.removeTaxRegistrationById(regId)))
    }
  }

  "Registration Progress" must {
    val registrationId = UUID.randomUUID.toString

    val corporationTaxRegistration = CorporationTaxRegistration(
      internalId = "intId",
      registrationID = registrationId,
      formCreationTimestamp = "testDateTime",
      language = LangConstants.english,
      registrationProgress = None
    )

    "be stored in the document correctly" in new Setup {
      setupCollection(repository, corporationTaxRegistration)

      def retrieve: Option[CorporationTaxRegistration] = await(repository.findOneBySelector(repository.regIDSelector(registrationId)))

      retrieve.get.registrationProgress mustBe None

      val progress = "In progress"
      await(repository.updateRegistrationProgress(registrationId, progress))

      retrieve.get.registrationProgress mustBe Some(progress)
    }
  }

  "updateRegistrationToHeld" must {

    val regId = "reg-12345"
    val dateTime = Instant.parse("2017-09-04T14:49:48.261Z")

    val validConfirmationReferences = ConfirmationReferences(
      acknowledgementReference = "BRCT12345678910",
      transactionId = "TX1",
      paymentReference = Some("PY1"),
      paymentAmount = Some("12.00")
    )

    val corporationTaxRegistration = CorporationTaxRegistration(
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
      contactDetails = Some(ContactDetails(Some("0123456789"), Some("0123456789"), Some("test@email.co.uk")
      )),
      tradingDetails = Some(TradingDetails("true")),
      status = RegistrationStatus.DRAFT,
      createdTime = dateTime,
      lastSignedIn = dateTime,
      groups = Some(Groups(
        groupRelief = true,
        nameOfCompany = Some(GroupCompanyName("testGroupName", GroupCompanyNameEnum.Other)),
        addressAndType = Some(GroupsAddressAndType(GroupAddressTypeEnum.ALF, BusinessAddress(
          "Line 1",
          "Line 2",
          Some("Telford"),
          Some("Shropshire"),
          Some("ZZ1 1ZZ"),
          None
        ))),
        Some(GroupUTR(Some("1234567890")))
      ))
    )

    "update registration status to held, set confirmation refs and remove trading details, contact details and company details" in new Setup {

      setupCollection(repository, corporationTaxRegistration)

      val result: Option[CorporationTaxRegistration] = await(repository.updateRegistrationToHeld(regId, validConfirmationReferences))

      val heldTs: Option[Instant] = result.flatMap(_.heldTimestamp)

      val expected: Option[CorporationTaxRegistration] = Some(CorporationTaxRegistration(
        internalId = "testID",
        registrationID = regId,
        formCreationTimestamp = "testDateTime",
        language = LangConstants.english,
        companyDetails = None,
        contactDetails = None,
        tradingDetails = None,
        status = RegistrationStatus.HELD,
        confirmationReferences = Some(validConfirmationReferences),
        createdTime = dateTime,
        lastSignedIn = dateTime,
        heldTimestamp = heldTs,
        groups = None
      ))

      result mustBe expected
    }
  }

  "retrieveLockedRegIds" must {

    val regId = "reg-12345"
    val lockedRegId = "reg-54321"
    val dateTime = Instant.parse("2017-09-04T14:49:48.261Z")

    val validConfirmationReferences = ConfirmationReferences(
      acknowledgementReference = "BRCT12345678910",
      transactionId = "TX1",
      paymentReference = Some("PY1"),
      paymentAmount = Some("12.00")
    )

    val corporationTaxRegistration = CorporationTaxRegistration(
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
      status = RegistrationStatus.DRAFT,
      createdTime = dateTime,
      lastSignedIn = dateTime
    )

    val lockedCorporationTaxRegistration = CorporationTaxRegistration(
      internalId = "testID",
      registrationID = lockedRegId,
      formCreationTimestamp = "testDateTime",
      language = LangConstants.english,
      companyDetails = Some(CompanyDetails(
        "testCompanyName",
        CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
        PPOB("MANUAL", Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"), None, "txid"))),
        "testJurisdiction"
      )),
      contactDetails = Some(ContactDetails(Some("0123456789"), Some("0123456789"), Some("test@email.co.uk")
      )),
      tradingDetails = Some(TradingDetails("true")),
      status = RegistrationStatus.LOCKED,
      createdTime = dateTime,
      lastSignedIn = dateTime
    )

    "retrieve only regids with the status of LOCKED" in new Setup {

      setupCollection(repository, corporationTaxRegistration)
      setupCollection(repository, lockedCorporationTaxRegistration)

      val result: Seq[String] = await(repository.retrieveLockedRegIDs())

      result mustBe List(lockedRegId)
    }
  }

  "retrieveStatusAndExistenceOfCTUTRByAckRef" must {

    val ackRef = "BRCT09876543210"

    def corporationTaxRegistration(status: String, ctutr: Boolean) = CorporationTaxRegistration(
      internalId = "testID",
      registrationID = "regId",
      formCreationTimestamp = "testDateTime",
      language = LangConstants.english,
      companyDetails = Some(CompanyDetails(
        "testCompanyName",
        CHROAddress("Premises", "Line 1", Some("Line 2"), "Country", "Locality", Some("PO box"), Some("Post code"), Some("Region")),
        PPOB("MANUAL", Some(PPOBAddress("10 test street", "test town", Some("test area"), Some("test county"), Some("XX1 1ZZ"), Some("test country"), None, "txid"))),
        "testJurisdiction"
      )),
      contactDetails = Some(ContactDetails(Some("0123456789"), Some("0123456789"), Some("test@email.co.uk")
      )),
      tradingDetails = Some(TradingDetails("true")),
      status = RegistrationStatus.ACKNOWLEDGED,
      confirmationReferences = Some(ConfirmationReferences(acknowledgementReference = ackRef, "txId", None, None)),
      acknowledgementReferences = Some(AcknowledgementReferences(Option("ctutr").filter(_ => ctutr), "timestamp", status)),
      createdTime = Instant.parse("2017-09-04T14:49:48.261Z"),
      lastSignedIn = Instant.parse("2017-09-04T14:49:48.261Z")
    )

    "retrieve nothing when registration is not found" in new Setup {
      val result: Option[(String, Boolean)] = await(repository.retrieveStatusAndExistenceOfCTUTR("Non-existent AckRef"))
      result mustBe None
    }
    "retrieve a CTUTR (as a Boolean) when the ackref status is 04" in new Setup {
      setupCollection(repository, corporationTaxRegistration("04", ctutr = true))

      val result: Option[(String, Boolean)] = await(repository.retrieveStatusAndExistenceOfCTUTR(ackRef))
      result mustBe Option("04" -> true)
    }
    "retrieve no CTUTR (as a Boolean) when the ackref status is 06" in new Setup {
      setupCollection(repository, corporationTaxRegistration("06", ctutr = false))

      val result: Option[(String, Boolean)] = await(repository.retrieveStatusAndExistenceOfCTUTR(ackRef))
      result mustBe Option("06" -> false)
    }
  }

  "updateRegistrationWithAdminCTReference" must {

    val ackRef = "BRCT09876543210"
    val dateTime = Instant.parse("2017-09-04T14:49:48.261Z")
    val regId = "regId"
    val confRefs = Some(ConfirmationReferences(acknowledgementReference = ackRef, "txId", None, None))
    val timestamp = now.toString

    def corporationTaxRegistration(status: String, ctutr: Boolean) = CorporationTaxRegistration(
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
      contactDetails = Some(ContactDetails(Some("0123456789"), Some("0123456789"), Some("test@email.co.uk")
      )),
      tradingDetails = Some(TradingDetails("true")),
      status = RegistrationStatus.DRAFT,
      confirmationReferences = confRefs,
      acknowledgementReferences = Some(AcknowledgementReferences(Option("ctutr").filter(_ => ctutr), timestamp, status)),
      createdTime = dateTime,
      lastSignedIn = dateTime
    )

    "update a registration with an admin ct reference" in new Setup {
      val newUtr = "newUtr"
      val newStatus = "04"

      setupCollection(repository, corporationTaxRegistration("06", ctutr = false))

      val result: Option[CorporationTaxRegistration] = await(repository.updateRegistrationWithAdminCTReference(ackRef, newUtr))

      result mustBe Some(corporationTaxRegistration("06", ctutr = false))
    }

    "not update a registration that does not exist" in new Setup {
      val newUtr = "newUtr"
      val newStatus = "04"

      val result: Option[CorporationTaxRegistration] = await(repository.updateRegistrationWithAdminCTReference(ackRef, newUtr))
      val expected: Option[CorporationTaxRegistration] = None

      result mustBe expected
    }
  }

  "retrieveSessionIdentifiers" must {
    val regId = "12345"
    val (defaultSID, defaultCID) = ("oldSessID", "oldCredId")

    val timestamp = now

    def corporationTaxRegistration(regId: String, alreadyHasSessionIds: Boolean = false) = CorporationTaxRegistration(
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
      contactDetails = Some(ContactDetails(Some("0123456789"), Some("0123456789"), Some("test@email.co.uk")
      )),
      tradingDetails = Some(TradingDetails("true")),
      status = RegistrationStatus.DRAFT,
      createdTime = timestamp,
      lastSignedIn = timestamp,
      sessionIdentifiers = if (alreadyHasSessionIds) Some(SessionIds(defaultSID, defaultCID)) else None
    )

    "retrieve session identifiers" when {
      "a document exists and has identifiers" in new Setup {
        setupCollection(repository, corporationTaxRegistration(regId, alreadyHasSessionIds = true))

        await(repository.retrieveSessionIdentifiers(regId)) mustBe Some(SessionIds(defaultSID, defaultCID))
      }

    }
    "return a None" when {
      "a document exists and does not have identifiers" in new Setup {
        setupCollection(repository, corporationTaxRegistration(regId))

        await(repository.retrieveSessionIdentifiers(regId)) mustBe None
      }
      "there is no Document" in new Setup {
        await(repository.retrieveSessionIdentifiers(regId)) mustBe None
      }
    }
  }

  "storeSessionIdentifiers" must {
    val regId = "132"
    val sessionId = "sessionId-12345"
    val credId = "authorisedId"
    val (defaultSID, defaultCID) = ("oldSessID", "oldCredId")

    val timestamp = now

    def corporationTaxRegistration(regId: String, alreadyHasSessionIds: Boolean = false) = CorporationTaxRegistration(
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
      contactDetails = Some(ContactDetails(Some("0123456789"), Some("0123456789"), Some("test@email.co.uk")
      )),
      tradingDetails = Some(TradingDetails("true")),
      status = RegistrationStatus.DRAFT,
      createdTime = timestamp,
      lastSignedIn = timestamp,
      sessionIdentifiers = if (alreadyHasSessionIds) Some(SessionIds(defaultSID, defaultCID)) else None
    )

    "successfully write encrpyted identifiers to mongo" when {

      "a document exists" in new Setup {
        setupCollection(repository, corporationTaxRegistration(regId))
        await(repository.findOneBySelector(repository.regIDSelector(regId))) map {
          doc => doc.sessionIdentifiers
        } mustBe Some(None)

        await(repository.storeSessionIdentifiers(regId, sessionId, credId))

        await(repository.findOneBySelector(repository.regIDSelector(regId))) map {
          doc => doc.sessionIdentifiers
        } mustBe Some(Some(SessionIds(sessionId, credId)))
      }
      "a document exists and already has old session Ids" in new Setup {

        setupCollection(repository, corporationTaxRegistration(regId, alreadyHasSessionIds = true))
        await(repository.findOneBySelector(repository.regIDSelector(regId))) map {
          doc => doc.sessionIdentifiers
        } mustBe Some(Some(SessionIds(defaultSID, defaultCID)))


        await(repository.storeSessionIdentifiers(regId, sessionId, credId))

        await(repository.findOneBySelector(repository.regIDSelector(regId))) map {
          doc => doc.sessionIdentifiers
        } mustBe Some(Some(SessionIds(sessionId, credId)))
      }
    }

    "unsuccessful" when {
      "no document exists" in new Setup {
        intercept[NoSuchElementException](await(repository.storeSessionIdentifiers(regId, sessionId, credId)))
      }
    }
  }

  "transactionIdSelector" must {
    "return a mapping between a documents transaction id and the value provided" in new Setup {
      val transactionId = "fakeTransId"
      val mapping: Bson = Filters.equal("confirmationReferences.transaction-id", transactionId)
      repository.transIdSelector(transactionId) mustBe mapping
    }
  }

  "updateTransactionId" must {
    "update a document with the new transaction id" when {
      "a document is present in the database" in new Setup {
        val regId = "registrationId"

        def corporationTaxRegistration(transId: String): CorporationTaxRegistration = CorporationTaxRegistration(
          internalId = "testID",
          registrationID = regId,
          formCreationTimestamp = "testDateTime",
          language = LangConstants.english,
          confirmationReferences = Some(ConfirmationReferences("ackRef", transId, None, None))
        )

        val updateFrom = "updateFrom"
        val updateTo = "updateTo"

        repository.insert(corporationTaxRegistration(updateFrom))
        await(repository.updateTransactionId(updateFrom, updateTo)) mustBe updateTo
        await(repository.findOneBySelector(repository.regIDSelector(regId))) flatMap {
          doc => doc.confirmationReferences
        } mustBe Some(ConfirmationReferences("ackRef", updateTo, None, None))
      }
    }
    "fail to update a document" when {
      "a document is not present in the database" in new Setup {
        val regId = "registrationId"

        val updateFrom = "updateFrom"
        val updateTo = "updateTo"

        intercept[RuntimeException](await(repository.updateTransactionId(updateFrom, updateTo)))
        await(repository.findOneBySelector(repository.regIDSelector(regId))) flatMap {
          doc => doc.confirmationReferences
        } mustBe None
      }
    }
  }

  "retrieveStaleDocuments" must {

    val registration90DaysOldDraft: CorporationTaxRegistration = corporationTaxRegistration(
      registrationStatus = "draft",
      lastSignedIn = now.minus(90, ChronoUnit.DAYS)
    )

    val registration91DaysOldLocked: CorporationTaxRegistration = corporationTaxRegistration(
      registrationStatus = "locked",
      lastSignedIn = now.minus(91, ChronoUnit.DAYS)
    )

    val registration90DaysOldLocked: CorporationTaxRegistration = corporationTaxRegistration(
      registrationStatus = "locked",
      lastSignedIn = now.minus(90, ChronoUnit.DAYS)
    )

    val registration90DaysOldHeld: CorporationTaxRegistration = corporationTaxRegistration(
      registrationStatus = "held",
      lastSignedIn = now.minus(90, ChronoUnit.DAYS)
    )

    val registration30DaysOldDraft: CorporationTaxRegistration = corporationTaxRegistration(
      registrationStatus = "draft",
      lastSignedIn = now.minus(30, ChronoUnit.DAYS)
    )

    val registration91DaysOldDraft: CorporationTaxRegistration = corporationTaxRegistration(
      registrationStatus = "draft",
      lastSignedIn = now.minus(91, ChronoUnit.DAYS)
    )

    val registration90DaysOldSubmitted: CorporationTaxRegistration = corporationTaxRegistration(
      status = "submitted",
      lastSignedIn = now.minus(90, ChronoUnit.DAYS)
    )

    val registration90DaysOldHeldWithPaymentRef: CorporationTaxRegistration = corporationTaxRegistration(
      status = "held",
      lastSignedIn = now.minus(90, ChronoUnit.DAYS)
    ).copy(confirmationReferences = Some(ConfirmationReferences(transactionId = "txId", paymentReference = Some("TEST_PAY_REF"), paymentAmount = None)))

    val registration90DaysOldLockedWithPaymentRef: CorporationTaxRegistration = corporationTaxRegistration(
      status = "locked",
      lastSignedIn = now.minus(90, ChronoUnit.DAYS)
    ).copy(confirmationReferences = Some(ConfirmationReferences(transactionId = "txId", paymentReference = Some("TEST_PAY_REF"), paymentAmount = None)))

    "return 0 documents" when {
      "trying to fetch 1 but the database contains no documents" in new Setup {
        await(repository.retrieveStaleDocuments(1, 90)) mustBe Nil
      }

      "the database contains 0 documents matching the query when trying to fetch 1" in new Setup {
        insert(registration90DaysOldSubmitted)

        await(repository.retrieveStaleDocuments(1, 91)) mustBe Nil
      }
      "the database contains documents not matching the query - held/locked with payment reference" in new Setup {
        insert(registration90DaysOldHeldWithPaymentRef)
        insert(registration90DaysOldLockedWithPaymentRef)

        await(repository.retrieveStaleDocuments(1, 91)) mustBe Nil
      }
      "the database contains documents not matching the query - held/locked without payment reference and held timestamp is 90 days old" in new Setup {
        insert(registration90DaysOldHeld.copy(heldTimestamp = Some(now.minus(90, ChronoUnit.DAYS))))
        insert(registration90DaysOldLocked.copy(heldTimestamp = Some(now.minus(90, ChronoUnit.DAYS))))

        await(repository.retrieveStaleDocuments(1, 91)) mustBe Nil
      }
    }

    "return the oldest document" when {
      "the database contains 1 document matching the query" in new Setup {
        insert(registration91DaysOldDraft)

        await(repository.retrieveStaleDocuments(1, 90)).head.lastSignedIn mustBe
          List(registration91DaysOldDraft).head.lastSignedIn
      }

      "the database contains multiple documents matching the query but the batch size was set to 1" in new Setup {
        insert(registration90DaysOldDraft)
        insert(registration91DaysOldDraft)

        await(repository.retrieveStaleDocuments(1, 90)) mustBe List(registration91DaysOldDraft)
      }
    }

    "return multiple documents in order of document age" when {
      "the database contains multiple documents matching the query" in new Setup {
        insert(registration91DaysOldDraft)
        insert(registration91DaysOldLocked)

        await(repository.retrieveStaleDocuments(2, 90)).toSet mustBe Set(registration91DaysOldLocked, registration91DaysOldDraft)
      }

      "the database contains multiple documents with the same time matching the query" in new Setup {
        insert(registration91DaysOldDraft)
        insert(registration91DaysOldLocked)

        await(repository.retrieveStaleDocuments(2, 90)) must contain theSameElementsAs List(registration91DaysOldDraft, registration91DaysOldLocked)
      }

      "1 of the documents match the query and 3 do not" in new Setup {
        count mustBe 0
        insert(registration90DaysOldDraft)
        insert(registration91DaysOldLocked)
        insert(registration30DaysOldDraft)
        insert(registration90DaysOldSubmitted)

        val res: Seq[CorporationTaxRegistration] = await(repository.retrieveStaleDocuments(5, 90))
        res.map(_.status) mustBe List("draft")
        res.size mustBe 1
        res mustBe List(registration91DaysOldLocked)
      }
    }

    "not fail and continue" when {
      "an invalid registration document is read" in new Setup {
        val incorrectRegistration: JsObject = ctRegistrationJson(
          regId = registration91DaysOldDraft.registrationID,
          lastSignedIn = registration91DaysOldDraft.lastSignedIn.toEpochMilli,
          malform = Some(Json.obj("registrationID" -> true))
        )
        repository.insertRaw(incorrectRegistration)
        insert(registration91DaysOldDraft)

        await(repository.retrieveStaleDocuments(2, 90)) mustBe List(registration91DaysOldDraft)
      }
    }
  }

  "returnGroupsBlock" must {
    "return some of groups when it exists in ct" in new Setup {
      val encryptedUTR: JsValue = CorporationTaxRegistrationFixture.instanceOfCrypto.wts.writes("1234567890")

      val fullGroupJsonEncryptedUTR: JsValue = Json.parse(
        s"""{
           |"groups": {
           |   "groupRelief": true,
           |   "nameOfCompany": {
           |     "name": "testGroupName",
           |     "nameType" : "Other"
           |   },
           |   "addressAndType" : {
           |     "addressType" : "ALF",
           |       "address" : {
           |         "line1": "1 abc",
           |         "line2" : "2 abc",
           |         "line3" : "3 abc",
           |         "line4" : "4 abc",
           |         "country" : "country A",
           |         "postcode" : "ZZ1 1ZZ"
           |     }
           |   },
           |   "groupUTR" : {
           |     "UTR" : $encryptedUTR
           |   }
           |  }
           |}
        """.stripMargin)

      val regWithGroups: JsObject = ctRegistrationJson(
        regId = "123",
        malform = Some(fullGroupJsonEncryptedUTR.as[JsObject])
      )
      repository.insertRaw(regWithGroups)
      count mustBe 1
      val res: Option[Groups] = await(repository.returnGroupsBlock("123"))
      res.get mustBe validGroupsModel

    }
    "return nothing when no groups block exists in ct doc" in new Setup {
      repository.insertRaw(ctRegistrationJson(regId = "123"))
      count mustBe 1
      val res: Option[Groups] = await(repository.returnGroupsBlock("123"))
      res mustBe None
    }
    "return exception when no ctDoc exists" in new Setup {
      count mustBe 0
      intercept[Exception](await(repository.returnGroupsBlock("123")))

    }
  }
  "deleteGroupsBlock" must {
    "return true if no exception occurred" in new Setup {
      val encryptedUTR: JsValue = CorporationTaxRegistrationFixture.instanceOfCrypto.wts.writes("1234567890")

      val fullGroupJsonEncryptedUTR: JsValue = Json.parse(
        s"""{
           |"groups": {
           |   "groupRelief": true,
           |   "nameOfCompany": {
           |     "name": "testGroupName",
           |     "nameType" : "Other"
           |   },
           |   "addressAndType" : {
           |     "addressType" : "ALF",
           |       "address" : {
           |         "line1": "1 abc",
           |         "line2" : "2 abc",
           |         "line3" : "3 abc",
           |         "line4" : "4 abc",
           |         "country" : "country A",
           |         "postcode" : "ZZ1 1ZZ"
           |     }
           |   },
           |   "groupUTR" : {
           |     "UTR" : $encryptedUTR
           |   }
           |  }
           |}
        """.stripMargin)
      val regWithGroups: JsObject = ctRegistrationJson(
        regId = "123",
        malform = Some(fullGroupJsonEncryptedUTR.as[JsObject])
      )

      repository.insertRaw(regWithGroups)
      count mustBe 1
      val res: Boolean = await(repository.deleteGroupsBlock("123"))
      res mustBe true
      count mustBe 1
      await(repository.returnGroupsBlock("123")) mustBe None
    }
    "return true if no groups block existed in the first place and a delete occurs" in new Setup {
      val regWithoutGroups: JsObject = ctRegistrationJson(
        regId = "123"
      )
      repository.insertRaw(regWithoutGroups)
      count mustBe 1
      val res: Boolean = await(repository.deleteGroupsBlock("123"))
      res mustBe true
      await(repository.returnGroupsBlock("123")) mustBe None
    }
    "throw exception if no doc is found" in new Setup {
      count mustBe 0
      intercept[Exception](await(repository.deleteGroupsBlock("123")))
    }
  }

  "updateGroupsBlock" must {
    "return updated group and update block if one exists already" in new Setup {
      val encryptedUTR: JsValue = CorporationTaxRegistrationFixture.instanceOfCrypto.wts.writes("1234567890")

      val fullGroupJsonEncryptedUTR: JsValue = Json.parse(
        s"""{
           |"groups": {
           |   "groupRelief": true,
           |   "nameOfCompany": {
           |     "name": "testGroupName",
           |     "nameType" : "Other"
           |   },
           |   "addressAndType" : {
           |     "addressType" : "ALF",
           |       "address" : {
           |         "line1": "1 abc",
           |         "line2" : "2 abc",
           |         "line3" : "3 abc",
           |         "line4" : "4 abc",
           |         "country" : "country A",
           |         "postcode" : "ZZ1 1ZZ"
           |     }
           |   },
           |   "groupUTR" : {
           |     "UTR" : $encryptedUTR
           |   }
           |  }
           |}
        """.stripMargin)
      val regWithGroups: JsObject = ctRegistrationJson(
        regId = "123",
        malform = Some(fullGroupJsonEncryptedUTR.as[JsObject])
      )
      repository.insertRaw(regWithGroups)
      count mustBe 1
      val res: Option[Groups] = await(repository.returnGroupsBlock("123"))
      res mustBe Some(validGroupsModel)
      val resOfUpdate: Groups = await(repository.updateGroups("123", Groups(groupRelief = false, None, None, None)))
      resOfUpdate mustBe Groups(groupRelief = false, None, None, None)
    }

    "return groups when an upsert occurs of the block" in new Setup {

      val encryptedUTR: JsValue = CorporationTaxRegistrationFixture.instanceOfCrypto.wts.writes("1234567890")
      val fullGroupJsonEncryptedUTR: JsValue = Json.parse(
        s"""{
           |   "groupRelief": true,
           |   "nameOfCompany": {
           |     "name": "testGroupName",
           |     "nameType" : "Other"
           |   },
           |   "addressAndType" : {
           |     "addressType" : "ALF",
           |       "address" : {
           |         "line1": "1 abc",
           |         "line2" : "2 abc",
           |         "line3" : "3 abc",
           |         "line4" : "4 abc",
           |         "country" : "country A",
           |         "postcode" : "ZZ1 1ZZ"
           |     }
           |   },
           |   "groupUTR" : {
           |     "UTR" : $encryptedUTR
           |   }
           |}
        """.stripMargin)
      insert(corporationTaxRegistration(regId = "123").copy(groups = None))
      count mustBe 1
      await(repository.returnGroupsBlock("123")) mustBe None
      val resOfUpdate: Groups = await(repository.updateGroups("123", validGroupsModel))
      resOfUpdate mustBe validGroupsModel
      val groups: Groups = await(repository.findOneBySelector(repository.regIDSelector("123"))).get.groups.get
      val groupJson: JsValue = Json.toJson(groups)(Groups.formats(MongoValidation, app.injector.instanceOf[CryptoSCRS]))
      groupJson mustBe fullGroupJsonEncryptedUTR
    }
    "return groups when same data is inserted twice" in new Setup {
      val encryptedUTR: JsValue = CorporationTaxRegistrationFixture.instanceOfCrypto.wts.writes("1234567890")

      val fullGroupJsonEncryptedUTR: JsValue = Json.parse(
        s"""{
           |"groups": {
           |   "groupRelief": true,
           |   "nameOfCompany": {
           |     "name": "testGroupName",
           |     "nameType" : "Other"
           |   },
           |   "addressAndType" : {
           |     "addressType" : "ALF",
           |       "address" : {
           |         "line1": "1 abc",
           |         "line2" : "2 abc",
           |         "line3" : "3 abc",
           |         "line4" : "4 abc",
           |         "country" : "country A",
           |         "postcode" : "ZZ1 1ZZ"
           |     }
           |   },
           |   "groupUTR" : {
           |     "UTR" : $encryptedUTR
           |   }
           |  }
           |}
        """.stripMargin)
      val regWithGroups: JsObject = ctRegistrationJson(
        regId = "123",
        malform = Some(fullGroupJsonEncryptedUTR.as[JsObject])
      )
      repository.insertRaw(regWithGroups)
      count mustBe 1
      val resOfUpdate: Groups = await(repository.updateGroups("123", validGroupsModel))
      resOfUpdate mustBe validGroupsModel
    }

    "return exception if regDoc doesnt exist" in new Setup {
      count mustBe 0
      intercept[Exception](await(repository.updateGroups("123", validGroupsModel)))
    }
  }

  "update" must {
    "update a document with the specified updates" when {
      "the document exists" in new Setup {
        val key = "takeoverDetails"
        val regId = "0123456789"
        val testData: CorporationTaxRegistration = corpTaxRegModel()
        implicit val formats: OFormat[CorporationTaxRegistration] = repository.formats
        val testJson: JsObject = Json.toJson(testData).as[JsObject]

        val res: Future[Option[CorporationTaxRegistration]] = for {
          _ <- Future.successful(repository.insertRaw(testJson))
          _ <- repository.update(repository.regIDSelector(regId), key, testTakeoverDetails)
          model <- repository.findOneBySelector(repository.regIDSelector(regId))
        } yield model

        await(res).get.takeoverDetails mustBe Some(testTakeoverDetailsModel)
      }
    }

    "throw a NoSuchElementException" when {
      "the document doesn't exist" in new Setup {
        val key = "takeoverDetails"
        val regId = "0123456789"
        count mustBe 0

        intercept[NoSuchElementException](
          await(repository.update(repository.regIDSelector(regId), key, testTakeoverDetails))
        )
      }
    }
  }

  "getRegistrationStats" must {
    "count for number of documents in the collection for each status" which {
      def buildDocumentWithStatus(status: String) = CorporationTaxRegistration(
        internalId = "testID",
        registrationID = registrationId,
        formCreationTimestamp = "testDateTime",
        language = LangConstants.english,
        status = status
      )

      val heldDoc = buildDocumentWithStatus(HELD)
      val draftDoc = buildDocumentWithStatus(DRAFT)
      val lockedDoc = buildDocumentWithStatus(LOCKED)
      val submittedDoc = buildDocumentWithStatus(SUBMITTED)
      val rejectedDoc = buildDocumentWithStatus(REJECTED)
      val acknowledgedDoc = buildDocumentWithStatus(ACKNOWLEDGED)
      val multipleValidDocuments =
        Seq(heldDoc, draftDoc, draftDoc, lockedDoc, lockedDoc, lockedDoc, submittedDoc,
          rejectedDoc, rejectedDoc, acknowledgedDoc, acknowledgedDoc, acknowledgedDoc)

      val invalidDoc1 = buildDocumentWithStatus("invalid")
      val invalidDoc2 = buildDocumentWithStatus("")
      val invalidDoc3 = buildDocumentWithStatus("HELD")
      val multipleInvalidDocuments =
        Seq(invalidDoc1, invalidDoc2, invalidDoc3)

      "returns the count for each status in a map, " +
        "ignoring any documents without a status or with a status that is not recognised" in new Setup {
        insertMultiple(multipleValidDocuments ++ multipleInvalidDocuments)
        val result: Map[String, Int] = await(repository.getRegistrationStats)

        result mustBe Map(
          "held" -> 1,
          "draft" -> 2,
          "locked" -> 3,
          "submitted" -> 1,
          "rejected" -> 2,
          "acknowledged" -> 3,
        )
      }
    }
  }

  "createdTime index and legacy cleanup" must {
    "create a TTL index on createdTime for 180 days" in new Setup {
      val indexDocs = await(repository.collection.withDocumentClass[Document]().listIndexes().toFuture())
      val maybeTtl = indexDocs.find(_.getString("name") == "createdTimeExpiry")

      maybeTtl mustBe defined
      val ttl = maybeTtl.get
        .get("expireAfterSeconds")
        .get
        .asNumber()
        .longValue()

      ttl mustBe 180L * 24 * 60 * 60
    }

    "persist createdTime as BSON DateTime for new documents" in new Setup {
      val regId = UUID.randomUUID().toString
      val createdTs = Instant.now().truncatedTo(ChronoUnit.MILLIS)

      val doc = newCTDoc.copy(registrationID = regId, createdTime = createdTs, lastSignedIn = createdTs)
      insert(doc)

      val rawDoc = await(
        repository.collection
          .withDocumentClass[Document]()
          .find(Filters.equal("registrationID", regId))
          .projection(Projections.include("createdTime"))
          .first()
          .toFutureOption()
      )

      rawDoc mustBe defined
      rawDoc.get.getDate("createdTime").toInstant mustBe createdTs
    }

    "read legacy createdTime stored as NumberLong" in new Setup {
      val regId = UUID.randomUUID().toString
      val createdTs = Instant.now().minus(200, ChronoUnit.DAYS).truncatedTo(ChronoUnit.MILLIS)

      val legacyRaw = ctRegistrationJson(
        regId = regId,
        createdTime = createdTs.toEpochMilli
      )

      repository.insertRaw(legacyRaw)

      val fetched = await(repository.findOneBySelector(repository.regIDSelector(regId)))
      fetched mustBe defined
      fetched.get.createdTime mustBe createdTs
    }

    "delete only stale legacy INT64 createdTime records up to N limit" in new Setup {
      val staleTs = Instant.now().minus(181, ChronoUnit.DAYS).toEpochMilli
      val freshTs = Instant.now().minus(10, ChronoUnit.DAYS).toEpochMilli

      val staleLegacy1 = ctRegistrationJson(regId = UUID.randomUUID().toString, createdTime = staleTs)
      val staleLegacy2 = ctRegistrationJson(regId = UUID.randomUUID().toString, createdTime = staleTs)
      val freshLegacy = ctRegistrationJson(regId = UUID.randomUUID().toString, createdTime = freshTs)

      repository.insertRaw(staleLegacy1)
      repository.insertRaw(staleLegacy2)
      repository.insertRaw(freshLegacy)

      insert(newCTDoc.copy(registrationID = UUID.randomUUID().toString, createdTime = Instant.now().minus(181, ChronoUnit.DAYS)))

      await(repository.deleteNStaleLegacyCreatedTimeData(1)).getDeletedCount mustBe 1L

      val staleLegacyCountAfter = await(
        repository.collection
          .withDocumentClass[Document]()
          .countDocuments(
            Filters.and(
              Filters.`type`("createdTime", BsonType.INT64),
              Filters.lt("createdTime", staleTs + 1) // still stale
            )
          ).toFuture()
      )

      staleLegacyCountAfter mustBe 1L
    }

    "not delete anything when deleteNStaleLegacyCreatedTimeData limit is <= 0" in new Setup {
      val staleTs = Instant.now().minus(181, ChronoUnit.DAYS).toEpochMilli
      repository.insertRaw(ctRegistrationJson(regId = UUID.randomUUID().toString, createdTime = staleTs))

      await(repository.deleteNStaleLegacyCreatedTimeData(0)).getDeletedCount mustBe 0L
      await(repository.deleteNStaleLegacyCreatedTimeData(-1)).getDeletedCount mustBe 0L
    }

    "delete all stale legacy INT64 records in batches and keep non-target records" in new Setup {
      val staleTs = Instant.now().minus(181, ChronoUnit.DAYS).toEpochMilli
      val freshTs = Instant.now().minus(5, ChronoUnit.DAYS).toEpochMilli
      val freshRegId = UUID.randomUUID().toString

      (1 to 25).foreach { _ =>
        repository.insertRaw(ctRegistrationJson(regId = UUID.randomUUID().toString, createdTime = staleTs))
      }

      repository.insertRaw(ctRegistrationJson(regId = freshRegId, createdTime = freshTs))

      val totalDeleted = await(repository.deleteAllStaleLegacyCreatedTimeData())
      totalDeleted mustBe 25L

      val remainingStaleLegacy = await(
        repository.collection
          .withDocumentClass[Document]()
          .countDocuments(
            Filters.and(
              Filters.`type`("createdTime", BsonType.INT64),
              Filters.lt("createdTime", Instant.now().minus(180, ChronoUnit.DAYS).toEpochMilli)
            )
          ).toFuture()
      )

      remainingStaleLegacy mustBe 0L

      val remainingFreshLegacy = await(
        repository.collection
          .withDocumentClass[Document]()
          .countDocuments(
            Filters.and(
              Filters.equal("registrationID", freshRegId),
            )
          ).toFuture()
      )

      remainingFreshLegacy mustBe 1L
    }
  }
}
