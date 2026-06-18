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

import audit.SubmissionEventDetail
import cats.implicits._
import connectors.{BusinessRegistrationConnector, BusinessRegistrationSuccessResponse, IncorporationInformationConnector}
import helpers.DateHelper
import models.RegistrationStatus.{ACKNOWLEDGED, DRAFT, LOCKED, SUBMITTED}
import models._
import models.api._
import models.validation.APIValidation
import play.api.libs.json.{JsObject, JsString, Json}
import play.api.mvc.{AnyContent, Request}
import repositories.{CorporationTaxRegistrationMongoRepository, Repositories, SequenceMongoRepository}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.audit.http.connector.AuditResult
import utils.{Logging, PagerDutyKeys, StringNormaliser}

import java.time.Instant
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class SubmissionServiceImpl @Inject()(val repositories: Repositories,
                                      val incorpInfoConnector: IncorporationInformationConnector,
                                      val submissionEventService: SubmissionEventService,
                                      val brConnector: BusinessRegistrationConnector,
                                      val corpTaxRegService: CorporationTaxRegistrationService,
                                      val auditService: AuditService
                                     )(implicit val ec: ExecutionContext) extends SubmissionService {
  lazy val cTRegistrationRepository: CorporationTaxRegistrationMongoRepository = repositories.cTRepository
  lazy val sequenceRepository: SequenceMongoRepository = repositories.sequenceRepository

  def instantNow: Instant = Instant.now()
}

trait SubmissionService extends DateHelper with Logging {

  implicit val ec: ExecutionContext
  val cTRegistrationRepository: CorporationTaxRegistrationMongoRepository
  val sequenceRepository: SequenceMongoRepository
  val incorpInfoConnector: IncorporationInformationConnector
  val submissionEventService: SubmissionEventService
  val auditService: AuditService
  val brConnector: BusinessRegistrationConnector
  val corpTaxRegService: CorporationTaxRegistrationService

  def instantNow: Instant

  def handleSubmission(rID: String, authProvId: String, handOffRefs: ConfirmationReferences, isAdmin: Boolean)
                      (implicit hc: HeaderCarrier, req: Request[AnyContent]): Future[ConfirmationReferences] = {
    cTRegistrationRepository.findOneBySelector(cTRegistrationRepository.regIDSelector(rID)) flatMap {
      case Some(doc) =>
        throwPagerDutyIfTxIDInDBDoesntMatchHandOffTxID(doc.confirmationReferences, handOffRefs.transactionId)
        if (doc.status == DRAFT || doc.status == LOCKED) {
          prepareDocumentForSubmission(rID, authProvId, handOffRefs, doc) flatMap { confRefs =>
            processPartialSubmission(rID, authProvId, confRefs, doc, isAdmin).ifM(
              ifTrue = Future.successful(confRefs),
              ifFalse = throw new RuntimeException(s"[handleSubmission] Failed to submit for regid: $rID")
            )
          }
        } else {
          doc.confirmationReferences match {
            case Some(existingRefs) if existingRefs != handOffRefs && confirmationRefsAndPaymentRefsAreEmpty(existingRefs) =>
              storeConfirmationReferencesAndUpdateStatus(rID, existingRefs.copy(paymentReference = handOffRefs.paymentReference, paymentAmount = handOffRefs.paymentAmount), None)

            case Some(existingRefs) =>
              logger.info(s"[handleSubmission] - Confirmation refs for Reg ID: $rID already exist")
              Future.successful(existingRefs)

            case _ =>
              logger.error(s"[handleSubmission] - Registration status is ${doc.status} for regId: $rID but confirmation refs not found")
              throw new RuntimeException(s"Registration status is held for regId: $rID but confirmation refs not found")
          }
        }
      case None => throw new RuntimeException(s"[handleSubmission] Registration Document not found for regId: $rID")
    }
  }

  private[services] def throwPagerDutyIfTxIDInDBDoesntMatchHandOffTxID(crConfRefs: Option[ConfirmationReferences], hOffTxID: String): Boolean = {
    crConfRefs.fold(false) {
      confRef =>
        if (confRef.transactionId != hOffTxID) {
          logger.error(s"${PagerDutyKeys.TXID_IN_CR_DOESNT_MATCH_HANDOFF_TXID} - CR txId: ${confRef.transactionId} hand off txId: $hOffTxID ")
          true
        } else {
          false
        }
    }
  }

  def updateCTRecordWithAckRefs(ackRef: String, apiNotification: AcknowledgementReferences): Future[Option[CorporationTaxRegistration]] = {
    cTRegistrationRepository.findOneBySelector(cTRegistrationRepository.ackRefSelector(ackRef)) flatMap {
      case Some(record) =>
        cTRegistrationRepository.updateCTRecordWithAcknowledgments(ackRef, record.copy(acknowledgementReferences = Some(apiNotification), status = RegistrationStatus.ACKNOWLEDGED)) map {
          _ => Some(record)
        }
      case None =>
        logger.info(s"[updateCTRecordWithAckRefs] : No record could not be found using this ackref")
        Future.successful(None)
    }
  }

  def generateAckRef: Future[String] = sequenceRepository.getNext("AcknowledgementID").map(ref => f"BRCT$ref%011d")

  def prepareDocumentForSubmission(rID: String, authProvId: String, refs: ConfirmationReferences, doc: CorporationTaxRegistration)
                                  (implicit hc: HeaderCarrier, req: Request[AnyContent]): Future[ConfirmationReferences] = {
    doc.confirmationReferences match {
      case None =>
        for {
          newlyGeneratedAckRef <- generateAckRef
          updatedRefs <- storeConfirmationReferencesAndUpdateStatus(rID, refs.copy(acknowledgementReference = newlyGeneratedAckRef), Some(LOCKED))
        } yield updatedRefs

      case Some(cr) if confirmationRefsAndPaymentRefsAreEmpty(cr) =>
        storeConfirmationReferencesAndUpdateStatus(rID, cr.copy(paymentReference = refs.paymentReference, paymentAmount = refs.paymentAmount), Some(LOCKED))

      case Some(cr) =>
        Future.successful(cr)

    }
  }

  private[services] def storeConfirmationReferencesAndUpdateStatus(regId: String, refs: ConfirmationReferences, status: Option[String]): Future[ConfirmationReferences] = {
    status.fold(cTRegistrationRepository.updateConfirmationReferences(regId, refs))(cTRegistrationRepository.updateConfirmationReferencesAndUpdateStatus(regId, refs, _)) map {
      case Some(_) => refs
      case None =>
        logger.error(s"[HO6] [storeConfirmationReferencesAndUpdateStatus] - Could not find a registration document for regId : $regId")
        throw new RuntimeException(s"[HO6] Could not update confirmation refs for regId: $regId - registration document not found")
    }
  }

  private[services] def processPartialSubmission(regId: String, authProvId: String, confRefs: ConfirmationReferences, doc: CorporationTaxRegistration, isAdmin: Boolean)
                                                (implicit hc: HeaderCarrier, req: Request[AnyContent]): Future[Boolean] = {
    for {
      brMetadata <- retrieveBRMetadata(regId, isAdmin)
      partialSubmission = buildPartialApiSubmission(regId, confRefs.acknowledgementReference, authProvId, brMetadata, doc)
      partialSubmissionAsJson = Json.toJson(partialSubmission).as[JsObject]
      _ <- incorpInfoConnector.registerInterest(regId, confRefs.transactionId)
      _ <- submitPartialToApi(regId, confRefs.acknowledgementReference, partialSubmissionAsJson, authProvId)
      _ = auditUserPartialSubmission(regId, authProvId, partialSubmissionAsJson, doc)
      success <- cTRegistrationRepository.updateRegistrationToHeld(regId, confRefs) map (_.isDefined)
    } yield success
  }


  private[services] def buildPartialApiSubmission(regId: String, ackRef: String, authProvId: String, brMetadata: BusinessRegistration, ctData: CorporationTaxRegistration)
                                                 (implicit hc: HeaderCarrier): InterimApiRegistration = {
    val (sessionID, credID): (String, String) = hc.sessionId match {
      case Some(sesID) => (sesID.value, authProvId)
      case None => ctData.sessionIdentifiers match {
        case Some(sessionIdentifiers) => (sessionIdentifiers.sessionId, sessionIdentifiers.credId)
        case None => throw new RuntimeException(s"[buildPartialApiSubmission] No session identifiers available for API submission")
      }
    }

    val companyDetails = ctData.companyDetails.getOrElse(throw new RuntimeException("[buildPartialApiSubmission] no company details found in ct doc when building partial Api submission"))
    val contactDetails = ctData.contactDetails.getOrElse(throw new RuntimeException("[buildPartialApiSubmission] no contact details found in ct doc when building partial Api submission"))
    val tradingDetails = ctData.tradingDetails.getOrElse(throw new RuntimeException("[buildPartialApiSubmission] no trading details found in ct doc when building partial Api submission"))
    val completionCapacity = CompletionCapacity(
      brMetadata.completionCapacity.getOrElse(throw new RuntimeException("[buildPartialApiSubmission] no completion Capacity found in br when building partial Api submission"))
    )

    val optPPOBAddress: Option[PPOBAddress] = companyDetails.ppob match {
      case PPOB(PPOB.RO, _) => corpTaxRegService.convertROToPPOBAddress(companyDetails.registeredOffice)
      case PPOB(_, address) => address
    }

    val businessAddress: Option[BusinessAddress] = optPPOBAddress map {
      address =>
        BusinessAddress(
          line1 = StringNormaliser.removeIllegalCharacters(address.line1),
          line2 = StringNormaliser.removeIllegalCharacters(address.line2),
          line3 = address.line3.map { line3 => StringNormaliser.removeIllegalCharacters(line3) },
          line4 = address.line4.map { line4 => StringNormaliser.removeIllegalCharacters(line4) },
          postcode = address.postcode,
          country = address.country
        )
    }

    def formatGroupsForSubmission: Option[Groups] = ctData.groups.map {
      og =>
        if (og.groupRelief) {
          val nameOfComp = og.nameOfCompany
            .getOrElse(throw new RuntimeException(s"formatGroupsForSubmission groups exists but name does not: $regId"))
          val address = og.addressAndType
            .getOrElse(throw new RuntimeException(s"formatGroupsForSubmission groups exists but address does not: $regId"))
          val formattedGroupAddress = GroupsAddressAndType(addressType = address.addressType, address = BusinessAddress(
            line1 = StringNormaliser.removeIllegalCharacters(address.address.line1),
            line2 = StringNormaliser.removeIllegalCharacters(address.address.line2),
            line3 = address.address.line3.map { line3 => StringNormaliser.removeIllegalCharacters(line3) },
            line4 = address.address.line4.map { line4 => StringNormaliser.removeIllegalCharacters(line4) },
            postcode = address.address.postcode,
            country = address.address.country
          ))
          val utr = og.groupUTR.getOrElse(throw new RuntimeException(s"formatGroupsForSubmission groups exists but utr block does not: $regId"))
          val nameFormatted = APIValidation.parentGroupNameValidator.reads(JsString(nameOfComp.name))
            .getOrElse(throw new RuntimeException(s"Parent group name saved does not pass Api validation: $regId"))
          Groups(
            og.groupRelief,
            Some(nameOfComp.copy(name = nameFormatted)),
            Some(formattedGroupAddress),
            Some(utr))
        } else Groups(groupRelief = false, None, None, None)
    }

    val businessContactDetails = BusinessContactDetails(contactDetails.phone, contactDetails.mobile, contactDetails.email)

    InterimApiRegistration(
      ackRef = ackRef,
      metadata = Metadata(
        sessionId = sessionID,
        credId = credID,
        language = brMetadata.language,
        submissionTs = instantNow,
        completionCapacity = completionCapacity
      ),
      interimCorporationTax = InterimCorporationTax(
        companyName = companyDetails.companyName,
        returnsOnCT61 = tradingDetails.regularPayments.toBoolean,
        businessAddress = businessAddress,
        businessContactDetails = businessContactDetails,
        groups = formatGroupsForSubmission,
        takeOver = ctData.takeoverDetails.map(_.withSanitisedFields)
      )
    )
  }

  private[services] def submitPartialToApi(regId: String, ackRef: String, partialSubmission: JsObject, authProvId: String)
                                          (implicit hc: HeaderCarrier): Future[HttpResponse] = {
    submissionEventService.ctSubmission(ackRef, partialSubmission, regId) recoverWith {
      case e =>
        hc.sessionId match {
          case Some(xSesID) =>
            logger.warn(s"[storePartialSubmission] Saved session identifiers for regId: $regId")
            cTRegistrationRepository.storeSessionIdentifiers(regId, xSesID.value, authProvId) map (throw e)
          case _ =>
            logger.warn(s"[storePartialSubmission] No session identifiers to save for regID: $regId")
            throw e
        }
    }
  }

  private[services] def auditUserPartialSubmission(regId: String, authProvId: String, partialSubmission: JsObject, doc: CorporationTaxRegistration)
                                                  (implicit hc: HeaderCarrier, req: Request[AnyContent]): Future[AuditResult] = {
    import PPOB.RO

    val ppob = doc.companyDetails.getOrElse(throw new RuntimeException(s"Could not retrieve Company Registration after API Submission for $regId")).ppob
    val (txID, uprn) = (ppob.addressType, ppob.address) match {
      case (RO, _) => (None, None)
      case (_, Some(address)) => (Some(address.txid), address.uprn)
      case (_, None) => (None, None)
    }

    auditService.sendEvent(
      auditType = "interimCTRegistrationDetails",
      detail = SubmissionEventDetail(regId, authProvId, txID, uprn, ppob.addressType, partialSubmission)
    )
  }

  private[services] def retrieveBRMetadata(regId: String, isAdmin: Boolean = false)(implicit hc: HeaderCarrier): Future[BusinessRegistration] = {
    (if (isAdmin) brConnector.adminRetrieveMetadata(regId) else brConnector.retrieveMetadataByRegId(regId)) flatMap {
      case BusinessRegistrationSuccessResponse(metadata) if metadata.registrationID == regId => Future.successful(metadata)
      case BusinessRegistrationSuccessResponse(metadata) if metadata.registrationID != regId =>
        Future.failed(new RuntimeException(s"[retrieveBRMetadata] ${metadata.registrationID} does not match $regId with isAdmin $isAdmin"))
      case _ => Future.failed(new RuntimeException("[retrieveBRMetadata] Could not find BR Metadata"))
    }
  }

  private def confirmationRefsAndPaymentRefsAreEmpty(refs: ConfirmationReferences): Boolean = refs.paymentReference.isEmpty && refs.paymentAmount.isEmpty

  def setupPartialForTopupOnLocked(transID: String)(implicit hc: HeaderCarrier, req: Request[AnyContent]): Future[Boolean] = {
    logger.info(s"[setupPartialForTopup] Trying to update locked document of txId: $transID to held for topup with incorp update")

    cTRegistrationRepository.findOneBySelector(cTRegistrationRepository.transIdSelector(transID)) flatMap {
      case Some(reg) =>
        (reg.sessionIdentifiers, reg.confirmationReferences) match {
          case _ if reg.status == SUBMITTED || reg.status == ACKNOWLEDGED =>
            logger.info(s"[setupPartialForTopup] Accepting incorporation update, registration already submitted for txID: $transID")
            Future.successful(true)
          case _ if reg.status != RegistrationStatus.LOCKED =>
            throw new RuntimeException(s"[setupPartialForTopup] Document status of txID: $transID was not locked, was ${reg.status}")
          case (Some(sIds), Some(confRefs)) =>
            processPartialSubmission(reg.registrationID, sIds.credId, confRefs, reg, isAdmin = true)
          case _ =>
            logger.warn(s"[setupPartialForTopup] No session identifiers or conf refs for registration with txID: $transID")
            throw NoSessionIdentifiersInDocument
        }
      case _ => throw new RuntimeException(s"[setupPartialForTopup] Could not find registration by txID: $transID")
    }
  }
}