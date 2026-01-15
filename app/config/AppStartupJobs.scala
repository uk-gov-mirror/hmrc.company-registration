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

package config

import models.TakeoverDetails
import org.apache.pekko.actor.ActorSystem
import play.api.Configuration
import repositories.CorporationTaxRegistrationMongoRepository
import services.{MetricsService, TakeoverDetailsService}
import utils.Logging

import java.util.Base64
import java.util.concurrent.TimeUnit
import javax.inject.Inject
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

class Startup @Inject() (appStartupJobs: AppStartupJobs, actorSystem: ActorSystem)(implicit val ec: ExecutionContext) {

  actorSystem.scheduler.scheduleOnce(FiniteDuration(1, TimeUnit.MINUTES))(appStartupJobs.runEverythingOnStartUp())
}

class AppStartupJobsImpl @Inject() (val config: Configuration,
                                    val ctRepo: CorporationTaxRegistrationMongoRepository,
                                    val takeoverDetailsService: TakeoverDetailsService,
                                    val metricsService: MetricsService)(implicit val ec: ExecutionContext)
    extends AppStartupJobs

trait AppStartupJobs extends Logging {

  implicit val ec: ExecutionContext
  val config: Configuration
  val ctRepo: CorporationTaxRegistrationMongoRepository
  val takeoverDetailsService: TakeoverDetailsService
  val metricsService: MetricsService

  private def startupStats: Future[Unit] =
    ctRepo.getRegistrationStats map { stats =>
      logger.info(s"[startupStats] $stats")
    }

  private def lockedRegIds: Future[Unit] =
    ctRepo.retrieveLockedRegIDs() map { regIds =>
      val message = regIds.map(rid => s" RegId: $rid")
      logger.info(s"[lockedRegIds] RegIds with locked status:$message")
    }

  def getCTCompanyName(rid: String): Future[Unit] =
    ctRepo.retrieveMultipleCorporationTaxRegistration(rid) map { list =>
      list foreach { ctDoc =>
        logger.info(
          s"[getCTCompanyName] " +
            s"status : ${ctDoc.status} - " +
            s"reg Id : ${ctDoc.registrationID} - " +
            s"Company Name : ${ctDoc.companyDetails.fold("")(companyDetails => companyDetails.companyName)} - " +
            s"Trans ID : ${ctDoc.confirmationReferences.fold("")(confRefs => confRefs.transactionId)}")
      }
    }

  def fetchDocInfoByRegId(regIds: Seq[String]): Future[Seq[Unit]] =
    Future.sequence(regIds.map { regId =>
      ctRepo.findOneBySelector(ctRepo.regIDSelector(regId)).map {
        case Some(doc) =>
          logger.info(
            s"""
               |[fetchDocInfoByRegId] regId: $regId,
               | status: ${doc.status},
               | lastSignedIn: ${doc.lastSignedIn},
               | confRefs: ${doc.confirmationReferences},
               | tradingDetails: ${doc.tradingDetails.isDefined},
               | contactDetails: ${doc.contactDetails.isDefined},
               | companyDetails: ${doc.companyDetails.isDefined},
               | accountingDetails: ${doc.accountingDetails.isDefined},
               | accountsPreparation: ${doc.accountsPreparation.isDefined},
               | crn: ${doc.crn.isDefined},
               | verifiedEmail: ${doc.verifiedEmail.isDefined}
            """.stripMargin
          )
        case _ => logger.info(s"[fetchDocInfoByRegId] No registration document found for $regId")
      }
    })

  private def fetchByAckRef(ackRefs: Seq[String]): Unit =
    for (ackRef <- ackRefs)
      ctRepo.findOneBySelector(ctRepo.ackRefSelector(ackRef)).map {
        case Some(doc) =>
          logger.info(
            s"[fetchDocInfoByRegId] Ack Ref: $ackRef, RegId: ${doc.registrationID}, Status: ${doc.status}, LastSignedIn: ${doc.lastSignedIn}, ConfRefs: ${doc.confirmationReferences}")
        case _ =>
          logger.info(s"[fetchDocInfoByRegId] No registration document found for $ackRef")
      }

  def updateTakeoverData(regIds: List[String]): Future[Seq[TakeoverDetails]] =
    Future.traverse(regIds) { regId =>
      logger.info(s" $regId has had its takeover section rectified to false")
      takeoverDetailsService.updateTakeoverDetailsBlock(regId, TakeoverDetails(replacingAnotherBusiness = false, None, None, None, None))
    }

  def runEverythingOnStartUp(): Future[Unit] = {
    logger.info("[runEverythingOnStartUp] Running Startup Jobs")
    lazy val regId = config.get[String]("companyNameRegID")
    getCTCompanyName(regId)

    lazy val base64TakeoverRegIds = config.get[String]("list-of-takeover-regids")
    lazy val listOfTakeoverRegIds = new String(Base64.getDecoder.decode(base64TakeoverRegIds), "UTF-8").split(",").toList
    updateTakeoverData(listOfTakeoverRegIds)

    lazy val base64RegIds = config.get[String]("list-of-regids")
    lazy val listOftxIDs  = new String(Base64.getDecoder.decode(base64RegIds), "UTF-8").split(",").toList
    fetchDocInfoByRegId(listOftxIDs)

    lazy val base64ackRefs = config.get[String]("list-of-ackrefs")
    lazy val listOfackRefs = new String(Base64.getDecoder.decode(base64ackRefs), "UTF-8").split(",").toList
    fetchByAckRef(listOfackRefs)

    metricsService.invoke

    startupStats
    lockedRegIds

  }
}
