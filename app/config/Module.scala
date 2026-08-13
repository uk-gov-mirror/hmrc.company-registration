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

import auth.{CryptoSCRS, CryptoSCRSImpl}
import com.google.inject.AbstractModule
import com.google.inject.name.Names
import connectors._
import controllers.test.{TestEndpointController, TestEndpointControllerImpl}
import jobs.{MissingIncorporationJob, RemoveStaleDocumentsJob, ScheduledJob}
import repositories.{CorporationTaxRegistrationMongoRepository, Repositories, SequenceMongoRepository, ThrottleMongoRepository}
import services._
import services.admin.{AdminService, AdminServiceImpl}
import utils.{AlertLogging, AlertLoggingImpl}

class Module extends AbstractModule {

  override def configure(): Unit = {
    bindConfig()
    bindRepositories()
    bindServices()
    bindConnectors()
    bindControllers()
    bindJobs()
  }

  private def bindJobs(): Unit = {
    bind(classOf[ScheduledJob]).annotatedWith(Names.named("missing-incorporation-job")).to(classOf[MissingIncorporationJob]).asEagerSingleton()
    bind(classOf[ScheduledJob]).annotatedWith(Names.named("remove-stale-documents-job")).to(classOf[RemoveStaleDocumentsJob]).asEagerSingleton()
    bind(classOf[AppStartupJobs]).to(classOf[AppStartupJobsImpl]).asEagerSingleton()
    bind(classOf[Startup]).asEagerSingleton()
  }

  private def bindConfig(): Unit = {
    bind(classOf[CryptoSCRS]).to(classOf[CryptoSCRSImpl]).asEagerSingleton()
    bind(classOf[AlertLogging]).to(classOf[AlertLoggingImpl]).asEagerSingleton()
  }

  private def bindControllers(): Unit = {
    bind(classOf[TestEndpointController]).to(classOf[TestEndpointControllerImpl]).asEagerSingleton()
  }

  private def bindServices(): Unit = {
    bind(classOf[AdminService]).to(classOf[AdminServiceImpl]).asEagerSingleton()
    bind(classOf[AuditService]).to(classOf[AuditServiceImpl]).asEagerSingleton()
    bind(classOf[AccountingDetailsService]).to(classOf[AccountingDetailsServiceImpl]).asEagerSingleton()
    bind(classOf[MetricsService]).to(classOf[MetricsServiceImpl]).asEagerSingleton()
    bind(classOf[CompanyDetailsService]).to(classOf[CompanyDetailsServiceImpl]).asEagerSingleton()
    bind(classOf[ContactDetailsService]).to(classOf[ContactDetailsServiceImpl]).asEagerSingleton()
    bind(classOf[GroupsService]).to(classOf[GroupsServiceImpl]).asEagerSingleton()
    bind(classOf[CorporationTaxRegistrationService]).to(classOf[CorporationTaxRegistrationServiceImpl]).asEagerSingleton()
    bind(classOf[CorporationTaxRegistrationService]).to(classOf[CorporationTaxRegistrationServiceImpl]).asEagerSingleton()
    bind(classOf[EmailService]).to(classOf[EmailServiceImpl]).asEagerSingleton()
    bind(classOf[PrepareAccountService]).to(classOf[PrepareAccountServiceImpl]).asEagerSingleton()
    bind(classOf[UserAccessService]).to(classOf[UserAccessServiceImpl]).asEagerSingleton()
    bind(classOf[ProcessIncorporationService]).to(classOf[ProcessIncorporationServiceImpl]).asEagerSingleton()
    bind(classOf[SubmissionService]).to(classOf[SubmissionServiceImpl]).asEagerSingleton()
  }

  private def bindConnectors(): Unit = {
    bind(classOf[IncorporationInformationConnector]).to(classOf[IncorporationInformationConnectorImpl]).asEagerSingleton()
    bind(classOf[BusinessRegistrationConnector]).to(classOf[BusinessRegistrationConnectorImpl]).asEagerSingleton()
    bind(classOf[SendEmailConnector]).to(classOf[SendEmailConnectorImpl]).asEagerSingleton()
    bind(classOf[IncorporationCheckAPIConnector]).to(classOf[IncorporationCheckAPIConnectorImpl]).asEagerSingleton()
  }

  private def bindRepositories(): Unit = {
    bind(classOf[CorporationTaxRegistrationMongoRepository]).asEagerSingleton()
    bind(classOf[SequenceMongoRepository]).asEagerSingleton()
    bind(classOf[ThrottleMongoRepository]).asEagerSingleton()
    bind(classOf[Repositories]).asEagerSingleton()
  }
}