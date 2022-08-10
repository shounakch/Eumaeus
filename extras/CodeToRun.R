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

options(andromedaTempFolder = "D:/andromedaTemp")
options(sqlRenderTempEmulationSchema = NULL)

maxCores <- 4

# # For bulk uploading synthetic outcomes:
# Sys.setenv("AWS_OBJECT_KEY" = "bulk")
# Sys.setenv("AWS_ACCESS_KEY_ID" = keyring::key_get("bulkUploadS3Key"))
# Sys.setenv("AWS_SECRET_ACCESS_KEY" = keyring::key_get("bulkUploadS3Secret"))
# Sys.setenv("AWS_BUCKET_NAME" = keyring::key_get("bulkUploadS3Bucket"))
# Sys.setenv("AWS_DEFAULT_REGION" = "us-east-1")
# Sys.setenv("AWS_SSE_TYPE" = "AES256")
# Sys.setenv("DATABASE_CONNECTOR_BULK_UPLOAD" = TRUE)

# Database details-----
# example: JnJ MDCR
cdmDatabaseSchema <- "cdm_truven_mdcr_v1838"
serverSuffix <- "truven_mdcr"
cohortDatabaseSchema <- "scratch_fbu2"
databaseId<- "MDCR"
databaseName <- "IBM Health MarketScan Medicare Supplemental and Coordination of Benefits Database"
databaseDescription <- "IBM Health MarketScan® Medicare Supplemental and Coordination of Benefits Database (MDCR) represents health services of retirees in the United States with primary or Medicare supplemental coverage through privately insured fee-for-service, point-of-service, or capitated health plans. These data include adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy). Additionally, it captures laboratory tests for a subset of the covered lives."
tablePrefix <- "eumaeus_mdcr"
outputFolder <- "E:/eumaeusTest_mdcr" # DONE


# fill out connection details ------------
conn <- DatabaseConnector::createConnectionDetails(
  dbms = "redshift",
  server = paste0(keyring::key_get("epi_server"), "/", !!serverSuffix),
  port = 5439,
  user = keyring::key_get("redshiftUser"),
  password = keyring::key_get("redshiftPassword"),
  extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory",
  pathToDriver = 'D:/Drivers')

#oracleTempSchema <- NULL
cohortTable = 'cohort_fbu2'


# # chort diagnostics -- do not run for now
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

execute(connectionDetails = conn,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        databaseId = databaseId,
        databaseName = databaseName,
        databaseDescription = databaseDescription,
        outputFolder = outputFolder,
        maxCores = maxCores,
        exposureIds = getExposuresOfInterest()$exposureId,
        verifyDependencies = TRUE,
        createCohorts = TRUE,
        createAllControls = TRUE,
        runCohortMethod = F,
        runSccs = TRUE,
        runCaseControl = F,
        runHistoricalComparator = TRUE,
        generateDiagnostics = F,
        computeCriticalValues = TRUE,
        createDbCharacterization = F,
        exportResults = TRUE)


# # upload results to database server if there is one that was already set up
# uploadResults(outputFolder = outputFolder,
#               privateKeyFileName = "c:/home/keyfiles/study-data-site-covid19.dat",
#               userName = "study-data-site-covid19")

# # JnJ specific code to store database version:
# source("extras/GetDatabaseVersion.R")
# version <- getDatabaseVersion(connectionDetails, cdmDatabaseSchema)
# readr::write_csv(version, file.path(outputFolder, "version.csv"))
