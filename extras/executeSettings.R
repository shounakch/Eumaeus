connectionDetails = connectionDetails
cdmDatabaseSchema = cdmDatabaseSchema
cohortDatabaseSchema = cohortDatabaseSchema
cohortTable = cohortTable
databaseId = databaseId
databaseName = databaseName
databaseDescription = databaseDescription
outputFolder = outputFolder
maxCores = maxCores
exposureIds = getExposuresOfInterest()$exposureId
verifyDependencies = FALSE
createCohorts = FALSE
createAllControls = TRUE
runCohortMethod = F
runSccs = TRUE
runCaseControl = F
runHistoricalComparator = TRUE
generateDiagnostics = F
computeCriticalValues = TRUE
createDbCharacterization = TRUE
exportResults = TRUE

# -------------- TESTING CONCURRENT COMPARATOR ON EUMAEUS --------------

# connectionDetails = connectionDetails
# cdmDatabaseSchema = cdmDatabaseSchema
# cohortDatabaseSchema = cohortDatabaseSchema
# cohortTable = cohortTable
# startDate = as.character(timePeriods$startDate[i])
# endDate = as.character(timePeriods$endDate[i])
# exposureId = exposureId
# outcomeIds = controls$outcomeId
# analysisId = i
# outputFolder = exposureFolder




# -------------------------- DETAILS WHEN RUNNING synthesizePositiveControls

# connectionDetails = connectionDetails
# cdmDatabaseSchema = cdmDatabaseSchema
# exposureDatabaseSchema = cohortDatabaseSchema
# exposureTable = cohortTable
# outcomeDatabaseSchema = cohortDatabaseSchema
# outcomeTable = cohortTable
# outputDatabaseSchema = cohortDatabaseSchema
# outputTable = cohortTable
# createOutputTable = FALSE
# exposureOutcomePairs = exposureOutcomePairs
# workFolder = synthesisFolder
# modelThreads = max(1, round(maxCores/8))
# generationThreads = min(6, maxCores)
# outputIdOffset = 10000
# firstExposureOnly = FALSE 
# firstOutcomeOnly = TRUE
# removePeopleWithPriorOutcomes = TRUE
# modelType = "survival"
# washoutPeriod = 365
# riskWindowStart = 1
# riskWindowEnd = 28
# addIntentToTreat = FALSE
# endAnchor = "cohort start"
# effectSizes = c(1.5, 2, 4)
# precision = 0.01
# prior = prior 
# control = control
# maxSubjectsForModel = 250000
# minOutcomeCountForModel = 25
# minOutcomeCountForInjection = 25
# covariateSettings = covariateSettings
# 
# 
# #---------------------------------
# 
# # library(dplyr)
# # library(ROhdsiWebApi)
# # 
# # # variables
# # serverRoot <- "" #my-prod-server.sjkenvsies.us-east-1.redshift.amazonaws.com
# # baseUrl <- "" #http://servername:port/WebAPI
# # 
# # # WebAPI authentication
# # authorizeWebApi(baseUrl, "windows") # Windows authentication
# # 
# # # List latest versions of data sources
# # cdmSources <- ROhdsiWebApi::getCdmSources(baseUrl = baseUrl) %>%
# #   dplyr::mutate(baseUrl = baseUrl,
# #                 dbms = 'redshift',
# #                 sourceDialect = 'redshift',
# #                 port = 5439,
# #                 version = .data$sourceKey %>% substr(., nchar(.) - 3, nchar(.)) %>% as.integer(),
# #                 database = .data$sourceKey %>% substr(., 5, nchar(.) - 6)) %>%
# #   dplyr::group_by(.data$database) %>%
# #   dplyr::arrange(dplyr::desc(.data$version)) %>%
# #   dplyr::mutate(sequence = dplyr::row_number()) %>%
# #   dplyr::ungroup() %>%
# #   dplyr::arrange(.data$database, .data$sequence) %>%
# #   dplyr::mutate(server = tolower(paste0(Sys.getenv("serverRoot"),"/", .data$database))) %>%
# #   dplyr::filter(sequence == 1)