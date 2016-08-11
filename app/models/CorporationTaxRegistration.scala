/*
 * Copyright 2016 HM Revenue & Customs
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

package models

import play.api.libs.json.Json

case class CorporationTaxRegistration(OID: String,
                                      registrationID: String,
                                      formCreationTimestamp: String,
                                      language: String,
                                      companyDetails: Option[CompanyDetails])

object CorporationTaxRegistration {
  implicit val formatRO = Json.format[ROAddress]
  implicit val formatPPOB = Json.format[PPOBAddress]
  implicit val formatCompanyDetails = Json.format[CompanyDetails]
  implicit val formats = Json.format[CorporationTaxRegistration]

  def empty: CorporationTaxRegistration = {
    CorporationTaxRegistration("", "", "", "", None)
  }
}



case class CorporationTaxRegistrationResponse(registrationID: String,
                                              formCreationTimestamp: String,
                                              language: String,
                                              link: Links)

object CorporationTaxRegistrationResponse {
  implicit val linksFormats = Json.format[Links]
  implicit val formats = Json.format[CorporationTaxRegistrationResponse]
}

case class CompanyDetails(companyName: String,
                          rOAddress: ROAddress,
                          pPOBAddress: PPOBAddress){

  def toCompanyDetailsResponse(registrationID: String): CompanyDetailsResponse = {
    CompanyDetailsResponse(
      companyName,
      rOAddress,
      pPOBAddress,
      Links.buildLinks(registrationID)
    )
  }
}

case class CompanyDetailsResponse(companyName: String,
                                  rOAddress: ROAddress,
                                  pPOBAddress: PPOBAddress,
                                  links: Links)

case class ROAddress(houseNameNumber: String,
                     addressLine1: String,
                     addressLine2: String,
                     addressLine3: String,
                     addressLine4: String,
                     postCode: String,
                     country: String)

case class PPOBAddress(houseNameNumber: String,
                       addressLine1: String,
                       addressLine2: String,
                       addressLine3: String,
                       addressLine4: String,
                       postCode: String,
                       country: String)

object CompanyDetails {
  implicit val formatRO = Json.format[ROAddress]
  implicit val formatPPOB = Json.format[PPOBAddress]
  implicit val formats = Json.format[CompanyDetails]
}

object CompanyDetailsResponse {
  implicit val formatRO = Json.format[ROAddress]
  implicit val formatPPOB = Json.format[PPOBAddress]
  implicit val formatLinks = Json.format[Links]
  implicit val formats = Json.format[CompanyDetailsResponse]
}

object ROAddress {
  implicit val format = Json.format[ROAddress]
}

object PPOBAddress {
  implicit val format = Json.format[PPOBAddress]
}

case class Language(lang: String)

object Language{
  implicit val format = Json.format[Language]
}

case class Links(self: String,
                 registration: String)

object Links {
  implicit val format = Json.format[Links]

  def buildLinks(registrationID: String): Links = {
    Links(
      self = s"/corporation-tax-registration/$registrationID/company-details",
      registration = s"corporation-tax-registration/$registrationID"
    )
  }
}

