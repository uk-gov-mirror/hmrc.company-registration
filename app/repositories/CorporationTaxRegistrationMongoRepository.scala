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

package repositories

import auth.{AuthorisationResource, CryptoSCRS}
import cats.data.OptionT
import cats.implicits._
import models._
import models.validation.MongoValidation
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters.{equal, lte}
import org.mongodb.scala.model.Indexes.ascending
import org.mongodb.scala.model.Updates.{set, unset}
import org.mongodb.scala.model._
import org.mongodb.scala.result.UpdateResult
import play.api.libs.json._
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}
import com.mongodb.client.result.DeleteResult
import org.bson.{BsonType, Document}
import org.mongodb.scala.model.Projections.include
import utils.Logging

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.util.concurrent.TimeUnit
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NoStackTrace

class MissingCTDocument(regId: String) extends NoStackTrace

@Singleton
class CorporationTaxRegistrationMongoRepository @Inject()(val mongo: MongoComponent,
                                                          crypto: CryptoSCRS
                                                         )(implicit val executionContext: ExecutionContext)
  extends PlayMongoRepository[CorporationTaxRegistration](
    mongoComponent = mongo,
    collectionName = "corporation-tax-registration-information",
    domainFormat = CorporationTaxRegistration.format(MongoValidation, crypto),
    indexes = Seq(
      IndexModel(
        ascending("registrationID"),
        IndexOptions()
          .name("RegIdIndex")
          .unique(false)
          .sparse(false)
      ),
      IndexModel(
        ascending("confirmationReferences.acknowledgement-reference"),
        IndexOptions()
          .name("AckRefIndex")
          .unique(false)
          .sparse(false)
      ),
      IndexModel(
        ascending("confirmationReferences.transaction-id"),
        IndexOptions()
          .name("TransIdIndex")
          .unique(false)
          .sparse(false)
      ),
      IndexModel(
        ascending("status", "heldTimestamp"),
        IndexOptions()
          .name("StatusHeldTimeIndex")
          .unique(false)
          .sparse(false)
      ),
      IndexModel(
        ascending("lastSignedIn"),
        IndexOptions()
          .name("LastSignedInIndex")
          .unique(false)
          .sparse(false)
      ),
      IndexModel(
        ascending("createdTime"),
        IndexOptions()
          .name("createdTimeExpiry")
          .expireAfter(180L, TimeUnit.DAYS)
      )
    ),
    extraCodecs = Seq(
      Codecs.playFormatCodec(implicitly[Format[JsValue]]),
      Codecs.playFormatCodec(implicitly[Format[JsObject]]),
      Codecs.playFormatCodec(implicitly[Format[JsString]]),
      Codecs.playFormatCodec(implicitly[Format[JsNumber]])
    )
  ) with AuthorisationResource[String] with Logging {

  implicit val formats: OFormat[CorporationTaxRegistration] = CorporationTaxRegistration.oFormat(CorporationTaxRegistration.format(MongoValidation, crypto))

  def regIDSelector(registrationID: String): Bson = equal("registrationID", registrationID)

  def transIdSelector(transactionId: String): Bson = equal("confirmationReferences.transaction-id", transactionId)

  def ackRefSelector(ackRef: String): Bson = equal("confirmationReferences.acknowledgement-reference", ackRef)

  def findAndUpdate(selector: Bson, modifier: Bson, fetchNewObject: Boolean): Future[Option[CorporationTaxRegistration]] =
    collection.findOneAndUpdate(
      selector,
      modifier,
      FindOneAndUpdateOptions()
        .upsert(false)
        .returnDocument(if (fetchNewObject) ReturnDocument.AFTER else ReturnDocument.BEFORE)
    ).headOption()

  def update(selector: Bson, key: String, value: JsValue): Future[UpdateResult] =
    collection.updateOne(
      selector,
      set(key, value),
      UpdateOptions().upsert(false)
    ).toFuture() map {
      case result if result.getMatchedCount > 0 => result
      case _ => throw new NoSuchElementException()
    }

  def replace(selector: Bson, replacement: CorporationTaxRegistration): Future[UpdateResult] =
    collection.replaceOne(
      selector,
      replacement,
      ReplaceOptions().upsert(false)
    ).toFuture()

  def unsetFields(selector: Bson, fields: String*): Future[UpdateResult] =
    collection.updateOne(
      selector,
      Updates.combine(fields.map(unset): _*),
      UpdateOptions().upsert(false)
    ).toFuture()

  def findOneBySelector(selector: Bson): Future[Option[CorporationTaxRegistration]] =
    collection.find(selector).headOption()

  def findAllBySelector(selector: Bson, limit: Int = Int.MaxValue): Future[Seq[CorporationTaxRegistration]] =
    collection.find(selector).limit(limit).toFuture()

  def retrieveMultipleCorporationTaxRegistration(registrationID: String): Future[Seq[CorporationTaxRegistration]] =
    findAllBySelector(regIDSelector(registrationID))

  def returnGroupsBlock(registrationID: String): Future[Option[Groups]] = {
    findOneBySelector(regIDSelector(registrationID))
      .map(ctDoc => ctDoc.getOrElse {
        throw new Exception("[returnGroupsBlock] ctDoc does not exist")
      }.groups)
  }

  def deleteGroupsBlock(registrationID: String): Future[Boolean] =
    unsetFields(regIDSelector(registrationID), "groups").map {
      case deleted if deleted.getMatchedCount > 0 => true
      case _ => throw new Exception(s"[deleteGroupsBlock] no Delete occurred Document was not found for regId: $registrationID")
    }

  def updateGroups(registrationID: String, groups: Groups): Future[Groups] = {
    val json = Json.toJson(groups)(Groups.formats(MongoValidation, crypto))
    update(regIDSelector(registrationID), "groups", json).map {
      updateWriteResult =>
        if (updateWriteResult.getMatchedCount == 1) {
          groups
        } else {
          throw new Exception(s"[updateGroups] failed for regId: $registrationID because a record was not found")
        }
    }
  }

  def updateLastSignedIn(regId: String, timestamp: Instant): Future[Instant] =
    update(regIDSelector(regId), "lastSignedIn", JsNumber(timestamp.toEpochMilli)).map(_ => timestamp)

  def updateCTRecordWithAcknowledgments(ackRef: String, ctRecord: CorporationTaxRegistration): Future[UpdateResult] =
    collection.replaceOne(ackRefSelector(ackRef), ctRecord, ReplaceOptions().upsert(false)).toFuture()

  def updateTransactionId(updateFrom: String, updateTo: String): Future[String] = {
    update(
      transIdSelector(updateFrom),
      "confirmationReferences.transaction-id",
      JsString(updateTo)
    ) map { res =>
      if (res.getModifiedCount == 0) {
        logger.error(s"[updateTransactionId] No document with transId: $updateFrom was found")
        throw new RuntimeException("Did not update transaction ID")
      } else {
        updateTo
      }
    } recover {
      case e =>
        logger.error(s"[updateTransactionId] Unable to update transId: $updateFrom to $updateTo", e)
        throw e
    }
  }

  def createCorporationTaxRegistration(ctReg: CorporationTaxRegistration): Future[CorporationTaxRegistration] =
    collection.insertOne(ctReg).toFuture().map(_ => ctReg)

  def getExistingRegistration(registrationID: String): Future[CorporationTaxRegistration] =
    findOneBySelector(regIDSelector(registrationID)).map {
      _.getOrElse {
        logger.warn(s"[getExistingRegistration] No Document Found for RegId: $registrationID")
        throw new MissingCTDocument(registrationID)
      }
    }

  def updateCompanyDetails(registrationID: String, companyDetails: CompanyDetails): Future[Option[CompanyDetails]] =
    OptionT(findOneBySelector(regIDSelector(registrationID))).semiflatMap { data =>
      replace(regIDSelector(registrationID), data.copy(companyDetails = Some(companyDetails))).map(_ => companyDetails)
    }.value

  def retrieveCompanyDetails(registrationID: String): Future[Option[CompanyDetails]] =
    OptionT(findOneBySelector(regIDSelector(registrationID))).subflatMap(_.companyDetails).value

  def retrieveAccountingDetails(registrationID: String): Future[Option[AccountingDetails]] =
    OptionT(findOneBySelector(regIDSelector(registrationID))).subflatMap(_.accountingDetails).value

  def updateAccountingDetails(registrationID: String, accountingDetails: AccountingDetails): Future[Option[AccountingDetails]] =
    OptionT(findOneBySelector(regIDSelector(registrationID))).semiflatMap { data =>
      replace(regIDSelector(registrationID), data.copy(accountingDetails = Some(accountingDetails))).map(_ => accountingDetails)
    }.value

  def retrieveTradingDetails(registrationID: String): Future[Option[TradingDetails]] =
    OptionT(findOneBySelector(regIDSelector(registrationID))).subflatMap(_.tradingDetails).value

  def updateTradingDetails(registrationID: String, tradingDetails: TradingDetails): Future[Option[TradingDetails]] =
    OptionT(findOneBySelector(regIDSelector(registrationID))).semiflatMap { data =>
      replace(regIDSelector(registrationID), data.copy(tradingDetails = Some(tradingDetails))).map(_ => tradingDetails)
    }.value

  def retrieveContactDetails(registrationID: String): Future[Option[ContactDetails]] =
    OptionT(findOneBySelector(regIDSelector(registrationID))).subflatMap(_.contactDetails).value

  def updateContactDetails(registrationID: String, contactDetails: ContactDetails): Future[Option[ContactDetails]] =
    OptionT(findOneBySelector(regIDSelector(registrationID))).semiflatMap { data =>
      replace(regIDSelector(registrationID), data.copy(contactDetails = Some(contactDetails))).map(_ => contactDetails)
    }.value

  def retrieveConfirmationReferences(registrationID: String): Future[Option[ConfirmationReferences]] =
    OptionT(findOneBySelector(regIDSelector(registrationID))).subflatMap(_.confirmationReferences).value

  def updateConfirmationReferences(registrationID: String, confirmationReferences: ConfirmationReferences): Future[Option[ConfirmationReferences]] =
    OptionT(findOneBySelector(regIDSelector(registrationID))).semiflatMap { data =>
      replace(regIDSelector(registrationID), data.copy(confirmationReferences = Some(confirmationReferences))).map(_ => confirmationReferences)
    }.value

  def updateConfirmationReferencesAndUpdateStatus(registrationID: String, confirmationReferences: ConfirmationReferences, status: String): Future[Option[ConfirmationReferences]] =
    OptionT(findOneBySelector(regIDSelector(registrationID))).semiflatMap { data =>
      replace(regIDSelector(registrationID), data.copy(confirmationReferences = Some(confirmationReferences), status = status)).map(_ => confirmationReferences)
    }.value

  def updateCompanyEndDate(registrationID: String, model: AccountPrepDetails): Future[Option[AccountPrepDetails]] =
    OptionT(findOneBySelector(regIDSelector(registrationID))).semiflatMap { data =>
      replace(regIDSelector(registrationID), data.copy(accountsPreparation = Some(model))).map(_ => model)
    }.value

  def updateSubmissionStatus(registrationID: String, status: String): Future[String] =
    findAndUpdate(regIDSelector(registrationID), set("status", status), fetchNewObject = true).map {
      case Some(data) => data.status
      case _ => throw new MissingCTDocument(registrationID)
    }

  def removeTaxRegistrationInformation(registrationId: String): Future[Boolean] = {
    val modifier = Updates.combine(unset("tradingDetails"), unset("contactDetails"), unset("companyDetails"))
    findAndUpdate(regIDSelector(registrationId), modifier, fetchNewObject = true) map {
      case Some(corpTaxModel) =>
        List(corpTaxModel.tradingDetails, corpTaxModel.contactDetails, corpTaxModel.companyDetails).forall(_.isEmpty)
      case _ =>
        throw new MissingCTDocument(registrationId)
    }
  }

  def removeUnnecessaryRegistrationInformation(registrationId: String): Future[Boolean] =
    unsetFields(
      regIDSelector(registrationId),
      "confirmationReferences",
      "accountingDetails",
      "accountsPreparation",
      "verifiedEmail",
      "companyDetails",
      "tradingDetails",
      "contactDetails"
    ).map(_ => true) recover {
      case e: Exception =>
        logger.warn(s"[removeUnnecessaryInformation] - an error occurred for regId: $registrationId with error: $e")
        false
    }

  def updateHeldToSubmitted(registrationId: String, crn: String, submissionTS: String): Future[Boolean] = {
    getExistingRegistration(registrationId) flatMap {
      ct =>
        import RegistrationStatus.SUBMITTED
        val updatedDoc = ct.copy(
          status = SUBMITTED,
          crn = Some(crn),
          submissionTimestamp = Some(submissionTS),
          accountingDetails = None,
          accountsPreparation = None
        )
        replace(regIDSelector(registrationId), updatedDoc).map(_ => true)
    }
  }

  def getInternalId(id: String): Future[(String, String)] =
    getExistingRegistration(id).map { c => c.registrationID -> c.internalId }

  def removeTaxRegistrationById(registrationId: String): Future[Boolean] =
    collection.deleteOne(regIDSelector(registrationId)).toFuture() map {
      case res if res.getDeletedCount > 0 => true
      case _ => throw new MissingCTDocument(registrationId)
    }

  def updateEmail(registrationId: String, email: Email): Future[Option[Email]] =
    OptionT(findOneBySelector(regIDSelector(registrationId))).semiflatMap { registration =>
      replace(regIDSelector(registrationId), registration.copy(verifiedEmail = Some(email))).map(_ => email)
    }.value

  def retrieveEmail(registrationId: String): Future[Option[Email]] =
    OptionT(findOneBySelector(regIDSelector(registrationId))).subflatMap(_.verifiedEmail).value

  def updateLanguage(registrationId: String, language: Language): Future[Option[Language]] =
    OptionT(findOneBySelector(regIDSelector(registrationId))).semiflatMap { registration =>
      replace(regIDSelector(registrationId), registration.copy(language = language.code)).map(_ => language)
    }.value

  def retrieveLanguage(registrationId: String): Future[Option[Language]] =
    OptionT(findOneBySelector(regIDSelector(registrationId))).subflatMap(ct => Some(Language(ct.language))).value

  def updateRegistrationProgress(regId: String, progress: String): Future[Option[String]] =
    OptionT(findOneBySelector(regIDSelector(regId))).semiflatMap { registration =>
      replace(regIDSelector(regId), registration.copy(registrationProgress = Some(progress))).map(_ => progress)
    }.value

  def getRegistrationStats: Future[Map[String, Int]] = {
    val statusCounts: Seq[Future[(String, Int)]] = RegistrationStatus.allStatuses.map { status =>
      collection.countDocuments(equal("status", status)).toFuture().map(count => status -> count.toInt)
    }

    Future.sequence(statusCounts).map(_.toMap)
  }

  def fetchHO6Information(regId: String): Future[Option[HO6RegistrationInformation]] =
    findOneBySelector(regIDSelector(regId)) map (_.map { reg =>
      HO6RegistrationInformation(reg.status, reg.companyDetails.map(_.companyName), reg.registrationProgress)
    })

  def fetchDocumentStatus(regId: String): OptionT[Future, String] = for {
    status <- OptionT(findOneBySelector(regIDSelector(regId))).map(_.status)
    _ = logger.info(s"[fetchDocumentStatus] status for reg id $regId is $status ")
  } yield status

  def updateRegistrationToHeld(regId: String, confRefs: ConfirmationReferences): Future[Option[CorporationTaxRegistration]] = {

    val modifier = Updates.combine(
      set("status", RegistrationStatus.HELD),
      set("confirmationReferences", Json.toJson(confRefs)),
      set("heldTimestamp", Json.toJson(CorporationTaxRegistration.now)),
      unset("tradingDetails"),
      unset("contactDetails"),
      unset("companyDetails"),
      unset("groups")
    )

    findAndUpdate(regIDSelector(regId), modifier, fetchNewObject = true) map {
      _ flatMap { reg =>
        (reg.status, reg.confirmationReferences, reg.tradingDetails, reg.contactDetails, reg.companyDetails, reg.heldTimestamp) match {
          case (RegistrationStatus.HELD, Some(cRefs), None, None, None, Some(_)) if cRefs == confRefs => Some(reg)
          case _ => None
        }
      }
    }
  }

  def retrieveAllWeekOldHeldSubmissions(): Future[Seq[CorporationTaxRegistration]] =
    findAllBySelector(Filters.and(
      equal("status", RegistrationStatus.HELD),
      lte("heldTimestamp", Instant.now().minus(7, ChronoUnit.DAYS).toEpochMilli)
    ))

  def retrieveLockedRegIDs(): Future[Seq[String]] =
    findAllBySelector(equal("status", RegistrationStatus.LOCKED)).map {
      _.map(_.registrationID)
    }

  def retrieveStatusAndExistenceOfCTUTR(ackRef: String): Future[Option[(String, Boolean)]] = {
    for {
      maybeRegistration <- findOneBySelector(ackRefSelector(ackRef))
    } yield {
      for {
        document <- maybeRegistration
        apiAckRefs <- document.acknowledgementReferences
      } yield {
        apiAckRefs.status -> apiAckRefs.ctUtr.isDefined
      }
    }
  }

  def updateRegistrationWithAdminCTReference(ackRef: String, ctUtr: String): Future[Option[CorporationTaxRegistration]] = {
    val timestamp = CorporationTaxRegistration.now.toString
    val ackRefs = AcknowledgementReferences(Some(ctUtr), timestamp, "04")

    val selector = equal("confirmationReferences.acknowledgement-reference", ackRef)
    val modifier =
      Updates.combine(
        set("status", RegistrationStatus.ACKNOWLEDGED),
        set("acknowledgementReferences", Json.toJson(ackRefs)(AcknowledgementReferences.format(MongoValidation, crypto)))
      )

    findAndUpdate(selector, modifier, fetchNewObject = false)
  }

  def storeSessionIdentifiers(regId: String, sessionId: String, credId: String): Future[Boolean] = {
    val json = Json.toJson(
      SessionIds(sessionId.format(crypto), credId))(SessionIds.format(crypto)
    )
    update(regIDSelector(regId), "sessionIdentifiers", json) map {
      _.getModifiedCount == 1
    }
  }

  def retrieveSessionIdentifiers(regId: String): Future[Option[SessionIds]] = {
    for {
      taxRegistration <- findOneBySelector(regIDSelector(regId))
    } yield taxRegistration flatMap (_.sessionIdentifiers)
  }

  def retrieveStaleDocuments(count: Int, storageThreshold: Int): Future[Seq[CorporationTaxRegistration]] = {

    val query = Filters.and(
      Filters.in("status", "draft", "held", "locked"),
      Filters.exists("confirmationReferences.payment-reference", exists = false),
      Filters.lt("lastSignedIn", LocalDateTime.now(ZoneOffset.UTC).withHour(0).minusDays(storageThreshold).toInstant(ZoneOffset.UTC)),
      Filters.or(
        Filters.exists("heldTimestamp", exists = false),
        Filters.lt("heldTimestamp", LocalDateTime.now(ZoneOffset.UTC).withHour(0).minusDays(storageThreshold).toInstant(ZoneOffset.UTC).toEpochMilli)
      )
    )

    val ascending = Sorts.ascending("lastSignedIn")

    collection.find[JsObject](query)
      .sort(ascending)
      .batchSize(count)
      .limit(count)
      .map { json =>
        json.validate[CorporationTaxRegistration] match {
          case JsSuccess(ctr, _) => Seq(ctr)
          case JsError(_) =>
            logger.error(s"[retrieveStaleDocuments] Mongo failed, problem occured in collect - could not parse document")
            Seq()
        }
      }
      .toFuture()
      .map(_.flatten)
  }

  private val CreatedTimeTtlDays: Long = 180

  private def staleLegacyCreatedTimeFilter: Bson = {
    val cutoff = Instant.now()
      .minus(CreatedTimeTtlDays, ChronoUnit.DAYS)
      .toEpochMilli

      Filters.and(
        Filters.`type`("createdTime", BsonType.INT64),
        Filters.lt("createdTime", cutoff)
      )
  }

  private def errorMessage(e: Throwable, allOrN: String): String =
    s"[MongoRemoveInvalidCreatedTimeData][delete${allOrN}StaleLegacyCreatedTimeData] " +
      s"Deletion of stale legacy 'createdTime' data failed. Error: $e"

  def deleteNStaleLegacyCreatedTimeData(nLimitForDeletion: Int): Future[DeleteResult] =
    if (nLimitForDeletion <= 0) {
      Future.successful(DeleteResult.acknowledged(0))
    } else {
      val filter = staleLegacyCreatedTimeFilter
      collection
        .withDocumentClass[Document]()
        .find(filter)
        .projection(include("_id"))
        .limit(nLimitForDeletion)
        .toFuture()
        .flatMap { documents =>
          val ids = documents.map(_.getObjectId("_id"))
          if (ids.isEmpty) {
            logger.warn("[MongoRemoveInvalidCreatedTimeData][deleteNStaleLegacyCreatedTimeData] " +
              "No stale legacy documents found.")
            Future.successful(DeleteResult.acknowledged(0))
          } else {
            collection
              .deleteMany(Filters.and(Filters.in("_id", ids: _*), filter))
              .toFuture()
              .map { result =>
                logger.warn("[MongoRemoveInvalidCreatedTimeData][deleteNStaleLegacyCreatedTimeData] " +
                  s"Number of DELETED stale documents: ${result.getDeletedCount}, limit: $nLimitForDeletion")
                result
              }
          }
        }
    }

  def deleteAllStaleLegacyCreatedTimeData(batchSize: Int = 1000): Future[Long] = {
    def loop(total: Long): Future[Long] =
      deleteNStaleLegacyCreatedTimeData(batchSize).flatMap { result =>
        val deleted = result.getDeletedCount
        if (deleted <= 0) Future.successful(total) else loop(total + deleted)
      }

    loop(0L).map { total =>
      logger.warn("[MongoRemoveInvalidCreatedTimeData][deleteAllStaleLegacyCreatedTimeData] " +
        s"Total DELETED stale documents: $total")
      total
    }
      .recover { case e: Throwable =>
        logger.error(errorMessage(e, "All"))
        0L
      }
  }
}
