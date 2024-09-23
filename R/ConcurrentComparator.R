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
  
  ccDataName = settings$ccDataName
  analysisId = settings$analysisId
  exposureId = settings$exposureId
  baseExposureId = settings$baseExposureId
  dataName = settings$dataName
  outputFolder = settings$outputFolder
  tPeriod = settings$tPeriod
  periodFolder = settings$periodFolder
  
  loadDir = paste0(outputFolder, 
                   "/ccData_e_", 
                   exposureId, 
                   "_t", 
                   tPeriod, 
                   ".zip")
  
  ccDataObject = Andromeda::loadAndromeda(fileName = loadDir)
  class(ccDataObject) <- "ConcurrentComparatorData"
  attr(class(ccDataObject), "package") <- "ConcurrentComparator"
  
  population <- ConcurrentComparator:::createStudyPopulation(ccDataObject, 
                                                             outcomeId = outcomeId)
  
  fit <- ConcurrentComparator:::fitOutcomeModel(population = population)
  
  # Evaluate llr
  
  fit <- ConcurrentComparator::fitOutcomeModel(population = population,
                                               profileGrid = c(as.numeric(fit$coefficients), 0),
                                               profileBounds = NULL)
  
  # llNull <- getCyclopsProfileLogLikelihood(
  #   object = fit,
  #   parm = "exposureTRUE",
  #   x = 0
  # )$value
  
  saveRDS(fit, paste0(periodFolder, "/fit_t", exposureId, "_c_", outcomeId, ".Rds"))
  
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
  
  #concurrentComparatorFolder <- file.path(outputFolder, "ConcurrentComparator")
  concurrentComparatorFolder <- file.path(outputFolder, "ConcurrentComparator_1-28Days")
  #concurrentComparatorFolder <- file.path(outputFolder, "ConcurrentComparator_1-28Days_MyoPeri")
  
  if (!file.exists(concurrentComparatorFolder))
    dir.create(concurrentComparatorFolder)
  
  #concurrentComparatorSummaryFile <- file.path(outputFolder, "ConcurrentComparatorSummary.csv")
  concurrentComparatorSummaryFile <- file.path(outputFolder, "ConcurrentComparatorSummary_1-28Days.csv")
  #concurrentComparatorSummaryFile <- file.path(outputFolder, "ConcurrentComparatorSummary_1-28Days_MyoPeri.csv")
  
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
      for(i in nrow(timePeriods):1) {
        periodEstimatesFile <- file.path(exposureFolder, sprintf("estimates_t%d.csv", timePeriods$seqId[i]))
        
        if (!file.exists(periodEstimatesFile)) {
          
          periodEstimates <- list()
          
          for(exposureId in exposures$exposureId) {
            
            ParallelLogger::logInfo(sprintf("Computing concurrent comparator estimates for exposure %s and period: %s", exposureId, timePeriods$label[i]))
            
            #TaR = 1-21 days, analysisId = 2
            # estimates <- computeConcurrentComparatorEstimates(connectionDetails = connectionDetails,
            #                                                   cdmDatabaseSchema = cdmDatabaseSchema,
            #                                                   cohortDatabaseSchema = cohortDatabaseSchema,
            #                                                   cohortTable = cohortTable,
            #                                                   startDate = as.character(timePeriods$startDate[i]),
            #                                                   endDate = as.character(timePeriods$endDate[i]),
            #                                                   exposureId = exposureId,
            #                                                   outcomeIds = controls$outcomeId,
            #                                                   analysisId = 2, #analysisId = 2 for 1-21 days vs 22-42 days
            #                                                   outputFolder = exposureFolder,
            #                                                   cluster = cluster,
            #                                                   dataName = dataName,
            #                                                   baseExposureId = baseExposureId,
            #                                                   tPeriod = i)
            
            #TaR = 1-28 days, analysisId = 3
            # estimates <- computeConcurrentComparatorEstimates(connectionDetails = connectionDetails,
            #                                                   cdmDatabaseSchema = cdmDatabaseSchema,
            #                                                   cohortDatabaseSchema = cohortDatabaseSchema,
            #                                                   cohortTable = cohortTable,
            #                                                   startDate = as.character(timePeriods$startDate[i]),
            #                                                   endDate = as.character(timePeriods$endDate[i]),
            #                                                   exposureId = exposureId,
            #                                                   analysisId = 3, #analysisId = 3 for 1-28 days vs 29-56 days
            #                                                   outcomeIds = controls$outcomeId,
            #                                                   outputFolder = exposureFolder,
            #                                                   cluster = cluster,
            #                                                   dataName = dataName,
            #                                                   baseExposureId = baseExposureId,
            #                                                   tPeriod = i)
            
            #For myo/pericarditis
            estimates <- computeConcurrentComparatorEstimates(connectionDetails = connectionDetails,
                                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                                              cohortDatabaseSchema = cohortDatabaseSchema,
                                                              cohortTable = cohortTable,
                                                              startDate = as.character(timePeriods$startDate[i]),
                                                              endDate = as.character(timePeriods$endDate[i]),
                                                              exposureId = exposureId,
                                                              outcomeIds = 2001,
                                                              analysisId = 3, #analysisId = 2 for 1-21 days vs 22-42 days
                                                              outputFolder = exposureFolder,
                                                              cluster = cluster,
                                                              dataName = dataName,
                                                              baseExposureId = baseExposureId,
                                                              tPeriod = i)
            
            periodEstimates[[length(periodEstimates) + 1]] <- estimates
            
          }
          
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
    
    ParallelLogger::stopCluster(cluster)
    
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
  
  ## Create time period folders
  
  periodFolder = paste0(outputFolder, "/ConcurrentComparatorOutput_t_", tPeriod)
  if (!file.exists(periodFolder)) {
    
    dir.create(periodFolder)
    
  }
  
  ccDataFileName = paste0(outputFolder, "/ccData_e_", exposureId, "_t", tPeriod, ".zip")
  
  if(analysisId == 2) {
    
    timeAtRiskStart = 1
    timeAtRiskEnd = 21
    washoutTime = 22
    
  } else if(analysisId == 3) {
    
    timeAtRiskStart = 1
    timeAtRiskEnd = 28
    washoutTime = 29
    
  } else {
    
    stop("CC analysisId not recognized!")
    
  }
  
  if(outcomeIds == c(668)) {
    
    outcomeTable = cohortTable
    
  } else {
    
    outcomeTable = "condition_era"
    
  }
  
  if(!file.exists(ccDataFileName)) {
    
    #TaR = 1-28 days, analysisId = 3, myo/peri
    ccData <- ConcurrentComparator:::getDbConcurrentComparatorData(connectionDetails = connectionDetails,
                                                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                                                   targetId = exposureId,
                                                                   outcomeIds = outcomeIds,
                                                                   studyStartDate = startDate,
                                                                   studyEndDate = endDate,
                                                                   exposureDatabaseSchema = cohortDatabaseSchema,
                                                                   exposureTable = cohortTable,
                                                                   outcomeDatabaseSchema = cohortDatabaseSchema,
                                                                   outcomeTable = outcomeTable,
                                                                   # outcomeTable = cohortTable,    #for outcome of interest
                                                                   #outcomeTable = "condition_era", #for negative controls
                                                                   timeAtRiskStart = timeAtRiskStart,
                                                                   timeAtRiskEnd = timeAtRiskEnd,
                                                                   washoutTime = washoutTime)
    
    saveDir = paste0(outputFolder, "/ccData_e_", exposureId, "_t", tPeriod, ".zip")
    
    Andromeda::saveAndromeda(ccData, fileName = saveDir, maintainConnection = TRUE)
    
  } else {
    
    ParallelLogger::logInfo(sprintf("cc Data file already exists"))
    
  }
  
  settings = list("ccDataName" = paste0("ccData_e_", exposureId, "_t", tPeriod),
                  "analysisId" = analysisId,
                  "exposureId" = exposureId,
                  "baseExposureId" = baseExposureId,
                  "dataName" = dataName,
                  "tPeriod" = tPeriod,
                  "outputFolder" = outputFolder,
                  "periodFolder" = periodFolder)
  
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
