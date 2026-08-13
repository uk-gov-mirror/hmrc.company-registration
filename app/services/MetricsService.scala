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

import com.codahale.metrics.{Counter, Gauge, Timer}
import jobs.{LockResponse, MongoLocked, ScheduledService, UnlockingFailed}
import repositories.{CorporationTaxRegistrationMongoRepository, Repositories}
import uk.gov.hmrc.mongo.lock.LockService
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.play.bootstrap.metrics.Metrics
import utils.Logging

import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class MetricsServiceImpl @Inject()(metricsInstance: Metrics,
                                   servicesConfig: ServicesConfig,
                                   val repositories: Repositories
                                  )(implicit val ec: ExecutionContext) extends MetricsService {

  override val metrics: Metrics = metricsInstance
  lazy val ctRepository: CorporationTaxRegistrationMongoRepository = repositories.cTRepository

  override val ctutrConfirmationCounter: Counter = metrics.defaultRegistry.counter("ctutr-confirmation-counter")

  override val retrieveAccountingDetailsCRTimer: Timer = metrics.defaultRegistry.timer("retrieve-accounting-details-from-CR-timer")
  override val updateAccountingDetailsCRTimer: Timer = metrics.defaultRegistry.timer("update-accounting-details-from-CR-timer")

  override val retrieveCompanyDetailsCRTimer: Timer = metrics.defaultRegistry.timer("retrieve-company-details-from-CR-timer")
  override val updateCompanyDetailsCRTimer: Timer = metrics.defaultRegistry.timer("update-company-details-from-CR-timer")

  override val retrieveContactDetailsCRTimer: Timer = metrics.defaultRegistry.timer("retrieve-contact-details-from-CR-timer")
  override val updateContactDetailsCRTimer: Timer = metrics.defaultRegistry.timer("update-contact-details-from-CR-timer")

  override val createCorporationTaxRegistrationCRTimer: Timer = metrics.defaultRegistry.timer("create-corporation-tax-registration-CR-timer")
  override val retrieveCorporationTaxRegistrationCRTimer: Timer = metrics.defaultRegistry.timer("retrieve-corporation-tax-registration-CR-timer")
  override val retrieveFullCorporationTaxRegistrationCRTimer: Timer = metrics.defaultRegistry.timer("retrieve-full-corporation-tax-registration-CR-timer")
  override val updateReferencesCRTimer: Timer = metrics.defaultRegistry.timer("update-references-CR-timer")
  override val retrieveConfirmationReferenceCRTimer: Timer = metrics.defaultRegistry.timer("retrieve-confirmation-reference-CR-timer")
  override val acknowledgementConfirmationCRTimer: Timer = metrics.defaultRegistry.timer("acknowledgement-confirmation-CR-timer")

  override val updateCompanyEndDateCRTimer: Timer = metrics.defaultRegistry.timer("update-company-end-date-CR-timer")

  override val retrieveTradingDetailsCRTimer: Timer = metrics.defaultRegistry.timer("retrieve-trading-details-CR-timer")
  override val updateTradingDetailsCRTimer: Timer = metrics.defaultRegistry.timer("update-trading-details-CR-timer")

  override val userAccessCRTimer: Timer = metrics.defaultRegistry.timer("user-access-CR-timer")

  override val desSubmissionCRTimer: Timer = metrics.defaultRegistry.timer("des-submission-CR-timer")
  override val hipSubmissionCRTimer: Timer = metrics.defaultRegistry.timer("hip-submission-CR-timer")
  override val etmpSubmissionCrtTimer: Timer =
    if (servicesConfig.getBoolean("features.hip")) hipSubmissionCRTimer else desSubmissionCRTimer

  lazy val lockoutTimeout: Int = servicesConfig.getInt("metrics-job.lockTimeout")

  lazy val lockKeeper: LockService = LockService(repositories.lockRepository, "metrics-job-lock", lockoutTimeout.seconds)
}

trait MetricsService extends ScheduledService[Either[Map[String, Int], LockResponse]] with Logging {

  implicit val ec: ExecutionContext
  val ctutrConfirmationCounter: Counter

  val retrieveAccountingDetailsCRTimer: Timer
  val updateAccountingDetailsCRTimer: Timer

  val retrieveCompanyDetailsCRTimer: Timer
  val updateCompanyDetailsCRTimer: Timer

  val retrieveContactDetailsCRTimer: Timer
  val updateContactDetailsCRTimer: Timer

  val createCorporationTaxRegistrationCRTimer: Timer
  val retrieveCorporationTaxRegistrationCRTimer: Timer
  val retrieveFullCorporationTaxRegistrationCRTimer: Timer
  val updateReferencesCRTimer: Timer
  val retrieveConfirmationReferenceCRTimer: Timer
  val acknowledgementConfirmationCRTimer: Timer

  val updateCompanyEndDateCRTimer: Timer

  val retrieveTradingDetailsCRTimer: Timer
  val updateTradingDetailsCRTimer: Timer

  val userAccessCRTimer: Timer

  val desSubmissionCRTimer: Timer
  val hipSubmissionCRTimer: Timer
  def etmpSubmissionCrtTimer: Timer

  val ctRepository: CorporationTaxRegistrationMongoRepository
  val lockKeeper: LockService

  protected val metrics: Metrics

  def invoke(implicit ec: ExecutionContext): Future[Either[Map[String, Int], LockResponse]] = {
    lockKeeper.withLock(updateDocumentMetrics()).map {
      case None => Right(MongoLocked)
      case Some(res) =>
        logger.info(s"[invoke] acquired lock and returned updateDocumentMetrics: $res")
        Left(res)
    }.recover {
      case e: Exception => logger.error(s"[invoke] Error running updateDocumentMetrics with message: ${e.getMessage}")
        Right(UnlockingFailed)
    }
  }

  def updateDocumentMetrics(): Future[Map[String, Int]] = {
    ctRepository.getRegistrationStats map {
      stats => {
        for ((status, count) <- stats) {
          recordStatusCountStat(status, count)
        }
        stats
      }
    }
  }

  private class MetricsDisabledException extends Throwable

  private def recordStatusCountStat(status: String, count: Int) = {
    val metricName = s"status-count-stat.$status"
    try {
      val gauge: Gauge[Int] = new Gauge[Int] {
        val getValue: Int = count
      }
      metrics.defaultRegistry.remove(metricName)
      metrics.defaultRegistry.register(metricName, gauge)
    } catch {
      case _: MetricsDisabledException =>
        logger.warn(s"[recordStatusCountStat] Metrics disabled - $metricName -> $count")
    }
  }

  def processDataResponseWithMetrics[T](timer: Timer.Context, success: Option[Counter] = None, failed: Option[Counter] = None)(f: => Future[T]): Future[T] = {
    f map { data =>
      timer.stop()
      success foreach (_.inc(1))
      data
    } recover {
      case e =>
        timer.stop()
        failed foreach (_.inc(1))
        throw e
    }
  }
}
