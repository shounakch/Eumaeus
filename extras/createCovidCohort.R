#code from ConcurrentComparator/vignettes/ExampleUsage.Rmd

library(Eumaeus)
library(dplyr)

options(andromedaTempFolder = "E:/andromedaTemp")
options(sqlRenderTempEmulationSchema = NULL)

# IBM_MDCR #covid cohort created
# cdmDatabaseSchema <- "cdm_truven_mdcr_v2755"
# serverSuffix <- "truven_mdcr"
# cohortDatabaseSchema <- "scratch_schatt77"
# cohortTable <- "cohort_schatt77_covid"
# databaseId<- "MDCR"
# databaseName <- "IBM Health MarketScan Medicare Supplemental and Coordination of Benefits Database"
# databaseDescription <- "IBM Health MarketScan® Medicare Supplemental and Coordination of Benefits Database (MDCR) represents health services of retirees in the United States with primary or Medicare supplemental coverage through privately insured fee-for-service, point-of-service, or capitated health plans. These data include adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy). Additionally, it captures laboratory tests for a subset of the covered lives."
# tablePrefix <- "eumaeus_mdcr"
# outputFolder <- "E:/Shounak_R/eumaeusTest_truven_mdcr_Shounak"

# IBM_MDCD #covid cohort created
# cdmDatabaseSchema <- "cdm_truven_mdcd_v2888" #v1714 earlier
# serverSuffix <- "truven_mdcd"
# cohortDatabaseSchema <- "scratch_schatt77"
# #cohortTable <- "cohort_schatt77_covid"
# databaseId<- "MDCD"
# databaseName <- "IBM Health MarketScan® Multi-State Medicaid Database"
# databaseDescription <- "IBM MarketScan® Multi-State Medicaid Database (MDCD) adjudicated US health insurance claims for Medicaid enrollees from multiple states and includes hospital discharge diagnoses, outpatient diagnoses and procedures, and outpatient pharmacy claims as well as ethnicity and Medicare eligibility. Members maintain their same identifier even if they leave the system for a brief period however the dataset lacks lab data."
# tablePrefix <- "eumaeus_mdcd"
# outputFolder <- "E:/Shounak_R/eumaeusTest_truven_mdcd_Shounak"

# #### Optum Pan-Therapeutic Electronic Health Records (Panther EHR) #covid cohort created
# cdmDatabaseSchema <- "cdm_optum_ehr_v2247"
# serverSuffix <- "optum_ehr"
# cohortDatabaseSchema <- "scratch_schatt77"
# cohortTable <- "cohort_schatt77_covid"
# databaseId<- "OptumEHR"
# databaseName <- "Optum Pan-Therapeutic Electronic Health Records (Panther EHR)"
# databaseDescription <- "Optum PanTher EHR is a multi-dimensional database containing information on outpatient visits, diagnostic procedures, medications, laboratory results, hospitalizations, clinical notes and patient outcomes primarily from IDNs."
# tablePrefix <- "eumaeus_optum_ehr"
# outputFolder <- "E:/Shounak_R/eumaeusTest_optum_ehr_Shounak"

# #### IBM MarketScan Commercial Claims and Encounters (CCAE) #covid cohort created
# cdmDatabaseSchema <- "cdm_truven_ccae_v2887"
# serverSuffix <- "truven_ccae"
# cohortDatabaseSchema <- "scratch_schatt77"
# cohortTable <- "cohort_schatt77_covid"
# databaseId<- "CCAE"
# databaseName <- "IBM MarketScan Commercial Claims and Encounters (CCAE)"
# databaseDescription <- "Represent data from individuals enrolled in United States employer-sponsored insurance health plans.  The data includes adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy) as well as enrollment data from large employers and health plans who provide private healthcare coverage to employees, their spouses, and dependents.  Additionally, it captures laboratory tests for a subset of the covered lives.  This administrative claims database includes a variety of fee-for-service, preferred provider organizations, and capitated health plans."
# tablePrefix <- "eumaeus_truven_ccae"
# outputFolder <- "E:/Shounak_R/eumaeusTest_truven_ccae_Shounak"

#### Optum Clinformatics Extended Data Mart - Date of Death (DOD) #covid cohort created
cdmDatabaseSchema <- "cdm_optum_extended_dod_v2882"
serverSuffix <- "optum_extended_dod"
cohortDatabaseSchema <- "scratch_schatt77"
cohortTable <- "cohort_schatt77_covid"
databaseId<- "DOD"
databaseName <- "Optum Clinformatics Extended Data Mart - Date of Death (DOD)"
databaseDescription <- "Optum Clinformatics Extended DataMart is an adjudicated US administrative health claims database for members of private health insurance, who are fully insured in commercial plans or in administrative services only (ASOs), Legacy Medicare Choice Lives (prior to January 2006), and Medicare Advantage (Medicare Advantage Prescription Drug coverage starting January 2006).  The population is primarily representative of commercial claims patients (0-65 years old) with some Medicare (65+ years old) however ages are capped at 90 years.  It includes data captured from administrative claims processed from inpatient and outpatient medical services and prescriptions as dispensed, as well as results for outpatient lab tests processed by large national lab vendors who participate in data exchange with Optum.  This dataset also provides date of death (month and year only) for members with both medical and pharmacy coverage from the Social Security Death Master File (however after 2011 reporting frequency changed due to changes in reporting requirements) and location information for patients is at the US state level."
tablePrefix <- "eumaeus_optum_extended_dod"
outputFolder <- "E:/Shounak_R/eumaeusTest_optum_extended_dod_Shounak"

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

cohortTable = cohortTable

#need to change cohortIds for covid vaccines - ask Marc later about this (7/15).
#for now, proceed in a ``dumb'' way and simply rewrite flu exclusion (21216) and zoster exclusion (21217) cohorts
#info for above can be found in Eumaeus/inst/settings/CohortsToCreate.csv
#this file was NOT updated with the new exposures
info <- list(
  list(cohortId = 21216, cohortName = "Covid19BNT126b2", fileName = "E:/Shounak_R/Eumaeus/inst/cohorts/ComirnatyCovid19.json"),
  list(cohortId = 21217, cohortName = "Covid19mRNA1273", fileName = "E:/Shounak_R/Eumaeus/inst/cohorts/SpikevaxCovid19.json"))

cohortDefinitionSet <- do.call(
  rbind,
  lapply(info, function(cohort) {
    cohortJson <- readChar(cohort$fileName, file.info(cohort$fileName)$size)
    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
    cohortSql <- CirceR::buildCohortQuery(cohortExpression,
                                          options = CirceR::createGenerateOptions(
                                            generateStats = FALSE))
    data.frame(
      cohortId = cohort$cohortId,
      cohortName = cohort$cohortName,
      sql = cohortSql,
      json = cohortJson,
      stringsAsFactors = FALSE
    )
  }))

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)

CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                    cohortDatabaseSchema = cohortDatabaseSchema,
                                    cohortTableNames = cohortTableNames,
                                    incremental = TRUE)

CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                   cohortTableNames = cohortTableNames,
                                   cohortDefinitionSet = cohortDefinitionSet,
                                   incremental = FALSE,
                                   incrementalFolder = ".")

#DatabaseConnector::disconnect(connectionDetails)
