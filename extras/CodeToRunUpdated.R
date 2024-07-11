# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of Eumaeus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(Eumaeus)
library(dplyr)

options(andromedaTempFolder = "E:/andromedaTemp")
options(sqlRenderTempEmulationSchema = NULL)

maxCores <- 16 #maxCores = 1 provides error; creates empty list that cannot be subsetted

# Database details-----
# # example: JnJ MDCR (should replace with relevant database info) #done for CC
cdmDatabaseSchema <- "cdm_truven_mdcr_v2755"
serverSuffix <- "truven_mdcr"
cohortDatabaseSchema <- "scratch_schatt77"
databaseId<- "MDCR"
databaseName <- "IBM Health MarketScan Medicare Supplemental and Coordination of Benefits Database"
databaseDescription <- "IBM Health MarketScan® Medicare Supplemental and Coordination of Benefits Database (MDCR) represents health services of retirees in the United States with primary or Medicare supplemental coverage through privately insured fee-for-service, point-of-service, or capitated health plans. These data include adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy). Additionally, it captures laboratory tests for a subset of the covered lives."
tablePrefix <- "eumaeus_mdcr"
outputFolder <- "E:/eumaeusTest_truven_mdcr_Shounak"
# 
# # another example: JnJ MDCD #done for CC, HC
# cdmDatabaseSchema <- "cdm_truven_mdcd_v2888" #v1714 earlier
# serverSuffix <- "truven_mdcd"
# cohortDatabaseSchema <- "scratch_schatt77"
# databaseId<- "MDCD"
# databaseName <- "IBM Health MarketScan® Multi-State Medicaid Database"
# databaseDescription <- "IBM MarketScan® Multi-State Medicaid Database (MDCD) adjudicated US health insurance claims for Medicaid enrollees from multiple states and includes hospital discharge diagnoses, outpatient diagnoses and procedures, and outpatient pharmacy claims as well as ethnicity and Medicare eligibility. Members maintain their same identifier even if they leave the system for a brief period however the dataset lacks lab data."
# tablePrefix <- "eumaeus_mdcd"
# outputFolder <- "E:/eumaeusTest_truven_mdcd_Shounak"

# #### Optum Pan-Therapeutic Electronic Health Records (Panther EHR) #done for CC, HC
# cdmDatabaseSchema <- "cdm_optum_ehr_v2247"
# serverSuffix <- "optum_ehr"
# cohortDatabaseSchema <- "scratch_schatt77"
# cohortTable <- "cohort_schatt77"
# databaseId<- "OptumEHR"
# databaseName <- "Optum Pan-Therapeutic Electronic Health Records (Panther EHR)"
# databaseDescription <- "Optum PanTher EHR is a multi-dimensional database containing information on outpatient visits, diagnostic procedures, medications, laboratory results, hospitalizations, clinical notes and patient outcomes primarily from IDNs."
# tablePrefix <- "eumaeus_optum_ehr"
# outputFolder <- "E:/eumaeusTest_optum_ehr_Shounak"

# #### IBM MarketScan Commercial Claims and Encounters (CCAE) #done for CC, HC
# cdmDatabaseSchema <- "cdm_truven_ccae_v2887"
# serverSuffix <- "truven_ccae"
# cohortDatabaseSchema <- "scratch_schatt77"
# cohortTable <- "cohort_schatt77"
# databaseId<- "CCAE"
# databaseName <- "IBM MarketScan Commercial Claims and Encounters (CCAE)"
# databaseDescription <- "Represent data from individuals enrolled in United States employer-sponsored insurance health plans.  The data includes adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy) as well as enrollment data from large employers and health plans who provide private healthcare coverage to employees, their spouses, and dependents.  Additionally, it captures laboratory tests for a subset of the covered lives.  This administrative claims database includes a variety of fee-for-service, preferred provider organizations, and capitated health plans."
# tablePrefix <- "eumaeus_truven_ccae"
# outputFolder <- "E:/eumaeusTest_truven_ccae_Shounak"

#### Optum Clinformatics Extended Data Mart - Date of Death (DOD) #done for CC
# cdmDatabaseSchema <- "cdm_optum_extended_dod_v2882"
# serverSuffix <- "optum_extended_dod"
# cohortDatabaseSchema <- "scratch_schatt77"
# cohortTable <- "cohort_schatt77"
# databaseId<- "DOD"
# databaseName <- "Optum Clinformatics Extended Data Mart - Date of Death (DOD)"
# databaseDescription <- "Optum Clinformatics Extended DataMart is an adjudicated US administrative health claims database for members of private health insurance, who are fully insured in commercial plans or in administrative services only (ASOs), Legacy Medicare Choice Lives (prior to January 2006), and Medicare Advantage (Medicare Advantage Prescription Drug coverage starting January 2006).  The population is primarily representative of commercial claims patients (0-65 years old) with some Medicare (65+ years old) however ages are capped at 90 years.  It includes data captured from administrative claims processed from inpatient and outpatient medical services and prescriptions as dispensed, as well as results for outpatient lab tests processed by large national lab vendors who participate in data exchange with Optum.  This dataset also provides date of death (month and year only) for members with both medical and pharmacy coverage from the Social Security Death Master File (however after 2011 reporting frequency changed due to changes in reporting requirements) and location information for patients is at the US state level."
# tablePrefix <- "eumaeus_optum_extended_dod"
# outputFolder <- "E:/eumaeusTest_optum_extended_dod_Shounak"

# ----------------------------------------------------------------------------------------------------------- #

# # fill out connection details ------------
# # example: JnJ epi server (should replace with relevant commands)
# conn <- DatabaseConnector::createConnectionDetails(
#   dbms = "redshift",
#   server = paste0(keyring::key_get("epi_server"), "/", !!serverSuffix), #what's the need for !!?
#   port = 5439,
#   user = keyring::key_get("redshiftUser"),
#   password = keyring::key_get("redshiftPassword"),
#   extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory",
#   pathToDriver = 'D:/Drivers')
# 
# DatabaseConnector::connect(conn)

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "redshift",
  server = paste0(keyring::key_get("redshiftServer"), "/", !!serverSuffix),
  #server = "rhealth-prod-5.cldcoxyrkflo.us-east-1.redshift.amazonaws.com/optum_ehr",
  port = 5439,
  user = keyring::key_get("redshiftUser"),
  password = keyring::key_get("redshiftPassword"),
  extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory",
  pathToDriver = "D:/drivers/")

DatabaseConnector::connect(connectionDetails)

## keyring usage: keyring::key_set_with_value("redShiftPassword", password = "password")

#oracleTempSchema <- NULL
cohortTable = 'cohort_schatt77'
#cohortTable = 'mrna_cohort'
#connectionDetails = conn

# # cohort diagnostics -- do not run for now
# 
# runCohortDiagnostics(connectionDetails = connectionDetails,
#                      cdmDatabaseSchema = cdmDatabaseSchema,
#                      cohortDatabaseSchema = cohortDatabaseSchema,
#                      cohortTable = cohortTable,
#                      databaseId = databaseId,
#                      databaseName = databaseName,
#                      databaseDescription = databaseDescription,
#                      outputFolder = outputFolder,
#                      createCohorts = TRUE,
#                      runCohortDiagnostics = TRUE)
# 
# CohortDiagnostics::preMergeDiagnosticsFiles(file.path(outputFolder, "cohortDiagnostics"))
# CohortDiagnostics::launchDiagnosticsExplorer(file.path(outputFolder, "cohortDiagnostics"))
# 

# the main execution function
# execute(connectionDetails = conn,
#         cdmDatabaseSchema = cdmDatabaseSchema,
#         cohortDatabaseSchema = cohortDatabaseSchema,
#         cohortTable = cohortTable,
#         databaseId = databaseId,
#         databaseName = databaseName,
#         databaseDescription = databaseDescription,
#         outputFolder = outputFolder,
#         maxCores = maxCores,
#         exposureIds = getExposuresOfInterest()$exposureId,
#         verifyDependencies = FALSE,
#         createCohorts = TRUE,
#         #createAllControls = TRUE,
#         runCohortMethod = F,
#         runSccs = TRUE,
#         runCaseControl = F,
#         runHistoricalComparator = TRUE,
#         generateDiagnostics = F,
#         computeCriticalValues = TRUE,
#         createDbCharacterization = TRUE,
#         exportResults = TRUE)

#following execute works (modified with ConcurrentComparator)

library(ConcurrentComparator)

source("E:/Shounak_R/Eumaeus/extras/MainCopy.R")
source("E:/Shounak_R/Eumaeus/R/ConcurrentComparator.R")

dataName = serverSuffix

# run executeSettings.R now

# execute2(connectionDetails = connectionDetails,
#          cdmDatabaseSchema = cdmDatabaseSchema,
#          cohortDatabaseSchema = cohortDatabaseSchema,
#          cohortTable = cohortTable,
#          databaseId = databaseId,
#          databaseName = databaseName,
#          databaseDescription = databaseDescription,
#          outputFolder = outputFolder,
#          maxCores = maxCores,
#          exposureIds = getExposuresOfInterest()$exposureId,
#          verifyDependencies = FALSE,
#          createCohorts = F,
#          createAllControls = F, #already created
#          runCohortMethod = F,
#          runSccs = F,
#          runCaseControl = F,
#          runHistoricalComparator = FALSE,
#          runConcurrentComparator = TRUE,
#          generateDiagnostics = F,
#          computeCriticalValues = FALSE,
#          createDbCharacterization = FALSE,
#          exportResults = FALSE,
#          dataName = dataName)


# # upload results to database server if there is one that was already set up
# uploadResults(outputFolder = outputFolder,
#               privateKeyFileName = "c:/home/keyfiles/study-data-site-covid19.dat",
#               userName = "study-data-site-covid19")

# # JnJ specific code to store database version:
# source("extras/GetDatabaseVersion.R")
# version <- getDatabaseVersion(connectionDetails, cdmDatabaseSchema)
# readr::write_csv(version, file.path(outputFolder, "version.csv"))
