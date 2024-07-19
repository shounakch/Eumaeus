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

clusterFunction <- function(outcomeId,
                            settings) {
  
  #ccData = readRDS(file = paste0("E:/eumaeusTest_optum_ehr_Shounak/ConcurrentComparator/e_21216/", ccDataName, ".RData"))
  
  ccDataName = settings$ccDataName
  analysisId = settings$analysisId
  exposureId = settings$exposureId
  baseExposureId = settings$baseExposureId
  dataName = settings$dataName
  outputFolder = settings$outputFolder
  tPeriod = settings$tPeriod
  
  #loadDir = paste0("E:/eumaeusTest_", dataName, "_Shounak/ConcurrentComparator/e_", baseExposureId, "/ccData_e_", exposureId, "_t", analysisId, ".zip")
  
  loadDir = paste0(outputFolder, "/ccData_e_", exposureId, "_t", tPeriod, ".zip")
  
  ccDataObject = Andromeda::loadAndromeda(fileName = loadDir)
  class(ccDataObject) <- "ConcurrentComparatorData"
  attr(class(ccDataObject), "package") <- "ConcurrentComparator"
  
  population <- ConcurrentComparator:::createStudyPopulation(ccDataObject, outcomeId = outcomeId)
  
  fit <- ConcurrentComparator:::fitOutcomeModel(population = population)
  
  # Evaluate llr
  
  fit <- ConcurrentComparator::fitOutcomeModel(population = population,
                                               profileGrid = c(as.numeric(fit$coefficients), 0),
                                               profileBounds = NULL)
  
  baseOutput = c(exposureId,
                 baseExposureId,
                 outcomeId,
                 analysisId,
                 fit$outcomeStatistics$subjects[2],
                 fit$outcomeStatistics$outcomes[2],
                 fit$outcomeStatistics$kPtYrs[2],
                 fit$outcomeStatistics$subjects[1],
                 fit$outcomeStatistics$outcomes[1],
                 fit$outcomeStatistics$kPtYrs[1])
  
  # output = c(exposureId,
  #            baseExposureId,
  #            outcomeId,
  #            analysisId,
  #            fit$outcomeStatistics$subjects[2],
  #            fit$outcomeStatistics$outcomes[2],
  #            fit$outcomeStatistics$kPtYrs[2],
  #            fit$outcomeStatistics$subjects[1],
  #            fit$outcomeStatistics$outcomes[1],
  #            fit$outcomeStatistics$kPtYrs[1],
  #            fit$treatmentEstimate$logRr,
  #            fit$treatmentEstimate$logLb95,
  #            fit$treatmentEstimate$logUb95,
  #            fit$treatmentEstimate$seLogRr)
  
  if((sum(fit$outcomeStatistics$outcomes) == 0) || (fit$status != "OK")) {
    
    output = c(baseOutput, rep(NA, 5))
    
  } else {
    
    if(fit$treatmentEstimate$logRr <= 0) {
      
      llr = 1e-6
      
    } else {
      
      llr = fit$logLikelihoodProfile$value[1] - fit$logLikelihoodProfile$value[2]
      
    }
    
    output = c(baseOutput, 
               fit$treatmentEstimate$logRr,
               fit$treatmentEstimate$logLb95,
               fit$treatmentEstimate$logUb95,
               fit$treatmentEstimate$seLogRr,
               llr)
    
  }
  
  return(output)
  
}

runConcurrentComparator <- function(connectionDetails,
                                    cdmDatabaseSchema,
                                    cohortDatabaseSchema,
                                    cohortTable,
                                    outputFolder,
                                    maxCores,
                                    dataName) {
  
  start <- Sys.time()
  
  concurrentComparatorFolder <- file.path(outputFolder, "ConcurrentComparator")
  
  if (!file.exists(concurrentComparatorFolder))
    dir.create(concurrentComparatorFolder)
  
  concurrentComparatorSummaryFile <- file.path(outputFolder, "ConcurrentComparatorSummary.csv")
  #if (!file.exists(concurrentComparatorSummaryFile)) {
  if(TRUE) {
    
    allControls <- Eumaeus:::loadAllControls(outputFolder)
    
    exposureCohorts <- Eumaeus:::loadExposureCohorts(outputFolder) %>%
      filter(.data$sampled == FALSE & .data$comparator == FALSE)
    
    baseExposureIds <- exposureCohorts %>%
      distinct(.data$baseExposureId) %>%
      pull()
    allEstimates <- list()
    
    cluster = ParallelLogger::makeCluster(maxCores)
    ParallelLogger::clusterRequire(cluster, "ConcurrentComparator")
    ParallelLogger::clusterRequire(cluster, "Eumaeus")
    ParallelLogger::clusterRequire(cluster, "Andromeda")
    
    #baseExposureId <- baseExposureIds[8] #to comment
    for (baseExposureId in baseExposureIds) {
      
      exposures <- exposureCohorts %>%
        filter(.data$baseExposureId == !!baseExposureId) 
      
      controls <- allControls %>%
        filter(.data$exposureId == baseExposureId)
      
      exposureFolder <- file.path(concurrentComparatorFolder, 
                                  sprintf("e_%s", baseExposureId))
      if (!file.exists(exposureFolder))
        dir.create(exposureFolder)
      
      timePeriods <- Eumaeus:::splitTimePeriod(startDate = controls$startDate[1], endDate = controls$endDate[1])
      #i <- 1
      #for (i in 1:nrow(timePeriods)) {
      for(i in nrow(timePeriods):1) {
        periodEstimatesFile <- file.path(exposureFolder, sprintf("estimates_t%d.csv", timePeriods$seqId[i]))
        
        # ## Run all studies
        # 
        # periodEstimates <- list()
        # exposureId <- exposures$exposureId[1]
        # ParallelLogger::logInfo(sprintf("Computing concurrent comparator estimates for exposure %s and period: %s", exposureId, timePeriods$label[i]))
        # estimates <- computeConcurrentComparatorEstimates(connectionDetails = connectionDetails,
        #                                                   cdmDatabaseSchema = cdmDatabaseSchema,
        #                                                   cohortDatabaseSchema = cohortDatabaseSchema,
        #                                                   cohortTable = cohortTable,
        #                                                   startDate = as.character(timePeriods$startDate[i]),
        #                                                   endDate = as.character(timePeriods$endDate[i]),
        #                                                   exposureId = exposureId,
        #                                                   outcomeIds = controls$outcomeId,
        #                                                   analysisId = i,
        #                                                   outputFolder = exposureFolder)
        # periodEstimates[[length(periodEstimates) + 1]] <- estimates
        # 
        # # for (exposureId in exposures$exposureId) {
        # #   ParallelLogger::logInfo(sprintf("Computing concurrent comparator estimates for exposure %s and period: %s", exposureId, timePeriods$label[i]))
        # #   estimates <- computeConcurrentComparatorEstimates(connectionDetails = connectionDetails,
        # #                                                     cdmDatabaseSchema = cdmDatabaseSchema,
        # #                                                     cohortDatabaseSchema = cohortDatabaseSchema,
        # #                                                     cohortTable = cohortTable,
        # #                                                     startDate = as.character(timePeriods$startDate[i]),
        # #                                                     endDate = as.character(timePeriods$endDate[i]),
        # #                                                     exposureId = exposureId,
        # #                                                     outcomeIds = controls$outcomeId,
        # #                                                     analysisId = i,
        # #                                                     outputFolder = exposureFolder)
        # #   periodEstimates[[length(periodEstimates) + 1]] <- estimates
        # # }
        # 
        # periodEstimates <- bind_rows(periodEstimates)
        # readr::write_csv(periodEstimates, periodEstimatesFile)
        
        #if (!file.exists(periodEstimatesFile)) {
        #if (!file.exists(periodEstimatesFile) || (length(exposures$exposureId) > 1)) {
        if(TRUE) {

          periodEstimates <- list()
          #exposureId <- exposures$exposureId[1]
          
          for(exposureId in exposures$exposureId) {
            
            ParallelLogger::logInfo(sprintf("Computing concurrent comparator estimates for exposure %s and period: %s", exposureId, timePeriods$label[i]))
            
            estimates <- computeConcurrentComparatorEstimates(connectionDetails = connectionDetails,
                                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                                              cohortDatabaseSchema = cohortDatabaseSchema,
                                                              cohortTable = cohortTable,
                                                              startDate = as.character(timePeriods$startDate[i]),
                                                              endDate = as.character(timePeriods$endDate[i]),
                                                              exposureId = exposureId,
                                                              outcomeIds = controls$outcomeId,
                                                              analysisId = 2, #analysisId = 2 for 1-21 days vs 22-42 days
                                                              outputFolder = exposureFolder,
                                                              cluster = cluster,
                                                              dataName = dataName,
                                                              baseExposureId = baseExposureId,
                                                              tPeriod = i)
            periodEstimates[[length(periodEstimates) + 1]] <- estimates
            
          }
          
          # for (exposureId in exposures$exposureId) {
          #   ParallelLogger::logInfo(sprintf("Computing concurrent comparator estimates for exposure %s and period: %s", exposureId, timePeriods$label[i]))
          #   estimates <- computeConcurrentComparatorEstimates(connectionDetails = connectionDetails,
          #                                                     cdmDatabaseSchema = cdmDatabaseSchema,
          #                                                     cohortDatabaseSchema = cohortDatabaseSchema,
          #                                                     cohortTable = cohortTable,
          #                                                     startDate = as.character(timePeriods$startDate[i]),
          #                                                     endDate = as.character(timePeriods$endDate[i]),
          #                                                     exposureId = exposureId,
          #                                                     outcomeIds = controls$outcomeId,
          #                                                     analysisId = i,
          #                                                     outputFolder = exposureFolder)
          #   periodEstimates[[length(periodEstimates) + 1]] <- estimates
          # }

          periodEstimates <- bind_rows(periodEstimates)
          readr::write_csv(periodEstimates, periodEstimatesFile)
        } else {
          periodEstimates <- Eumaeus:::loadEstimates(periodEstimatesFile)
        }
        periodEstimates$seqId <- timePeriods$seqId[i]
        periodEstimates$period <- timePeriods$label[i]
        allEstimates[[length(allEstimates) + 1]] <- periodEstimates
      }
    }
    
    allEstimates <- bind_rows(allEstimates)  
    readr::write_csv(allEstimates, concurrentComparatorSummaryFile)
    
    #ParallelLogger::stopCluster(cluster)
    
  }
  
  delta <- Sys.time() - start
  message(paste("Completing all concurrent comparator analyses took", signif(delta, 3), attr(delta, "units")))
  
}

computeConcurrentComparatorEstimates <- function(connectionDetails,
                                                 cdmDatabaseSchema,
                                                 cohortDatabaseSchema,
                                                 cohortTable,
                                                 startDate,
                                                 endDate,
                                                 exposureId,
                                                 outcomeIds,
                                                 analysisId,
                                                 outputFolder,
                                                 cluster,
                                                 dataName,
                                                 baseExposureId,
                                                 tPeriod) {
  
  start <- Sys.time()
  
  ccData <- ConcurrentComparator:::getDbConcurrentComparatorData(connectionDetails = connectionDetails,
                                                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                                                 targetId = exposureId,
                                                                 #outcomeIds = 668,
                                                                 outcomeIds = outcomeIds,
                                                                 studyStartDate = startDate,
                                                                 #studyEndDate = "2021-06-30",
                                                                 studyEndDate = endDate,
                                                                 exposureDatabaseSchema = cohortDatabaseSchema,
                                                                 exposureTable = cohortTable,
                                                                 outcomeDatabaseSchema = cohortDatabaseSchema,
                                                                 #outcomeTable = cohortTable,    #for outcome of interest
                                                                 outcomeTable = "condition_era", #for negative controls
                                                                 timeAtRiskStart = 1,
                                                                 timeAtRiskEnd = 21,
                                                                 washoutTime = 22)
  
  #saveDir = paste0("E:/eumaeusTest_", dataName, "_Shounak/ConcurrentComparator/e_", baseExposureId, "/ccData_e_", exposureId, "_t", analysisId, ".zip")
  
  saveDir = paste0(outputFolder, "/ccData_e_", exposureId, "_t", tPeriod, ".zip")
  
  #Andromeda::saveAndromeda(ccData, fileName = paste0("E:/eumaeusTest_optum_ehr_Shounak/ConcurrentComparator/e_", exposureId, "/ccData_t", analysisId, ".zip"), maintainConnection = TRUE)
  Andromeda::saveAndromeda(ccData, fileName = saveDir, maintainConnection = TRUE)
  
  settings = list("ccDataName" = paste0("ccData_e_", exposureId, "_t", tPeriod),
                  "analysisId" = analysisId,
                  "exposureId" = exposureId,
                  "baseExposureId" = baseExposureId,
                  "dataName" = dataName,
                  "tPeriod" = tPeriod,
                  "outputFolder" = outputFolder)
  
  estimates <- ParallelLogger::clusterApply(cluster, outcomeIds, clusterFunction, 
                                            settings = settings)
  
  estimates = do.call(rbind, estimates)
  colnames(estimates) = c("exposureId",
                          "baseExposureId",
                          "outcomeId",
                          "analysisId",
                          "targetSubjects",
                          "targetOutcomes",
                          "targetYears",
                          "comparatorSubjects",
                          "comparatorOutcomes",
                          "comparatorYears",
                          "logRr",
                          "logLb95",
                          "logUb95",
                          "seLogRr",
                          "llr")
  
  estimates = as.data.frame(estimates)
  
  delta <- Sys.time() - start
  message(paste("Computing concurrent comparator estimates took", signif(delta, 3), attr(delta, "units")))
  
  return(estimates)
  
}
