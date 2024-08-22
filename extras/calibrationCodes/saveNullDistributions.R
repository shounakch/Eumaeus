#provide subset with data source * exposure * method * analysisId * timePeriod
#outputs null distribution for given outcomeId, using other outcomeIds
library(dplyr)
library(EmpiricalCalibration)

saveNullDistributionsClusterize <- function(outcomeId, settings) {
  
  subset = settings$subset
  nMCMC = settings$nMCMC
  writeFolder = settings$writeFolder
  exposureId = settings$exposureId
  
  file.name = paste0(writeFolder, "/nullFit_outcomeId=", outcomeId, "_exposureId=", exposureId,  ".Rds")
  
  #if(!file.exists(file.name)) {
  if(TRUE) {
    
    subsetWithoutOutcome <- subset %>% filter(outcomeId != !!outcomeId)
    
    if(sum(!is.na(subsetWithoutOutcome$seLogRr)) >= 5) {
      
      legitIndices <- abs(subsetWithoutOutcome$logRr) <= 5
      if(length(legitIndices) >= 5) {
        
        nullModel <- EmpiricalCalibration::fitNull(subsetWithoutOutcome$logRr[legitIndices],
                                                   subsetWithoutOutcome$seLogRr[legitIndices])
        
        if(nullModel[2] <= 0.01) {
          
          nullModelMcmc <- EmpiricalCalibration::fitMcmcNull(subsetWithoutOutcome$logRr[legitIndices],
                                                             subsetWithoutOutcome$seLogRr[legitIndices],
                                                             nMCMC)
          
          # nullMean <- nullModelMcmc[1]
          # nullSd <- 1 / sqrt(nullModelMcmc[2])
          
          finalNullModel <- nullModelMcmc
          
        } else {
          
          # nullMean <- nullModel[1]
          # nullSd <- nullModel[2]
          
          finalNullModel <- nullModel
          
        }
        
        # nullObject <- list("nullMean" = nullMean,
        #                    "nullSd" = nullSd)
        
      } else {
        
        # nullObject <- list("nullMean" = NA,
        #                    "nullSd" = NA)
        
        finalNullModel <- NA
        
      }
      
    } else {
      
      # nullObject <- list("nullMean" = NA,
      #                    "nullSd" = NA)
      
      finalNullModel <- NA
      
    }
    
    saveRDS(finalNullModel, file.name)
    
  }
  
}

saveNullDistributions <- function(databaseId,
                                  exposureId,
                                  methodName,
                                  analysisId,
                                  tPeriod,
                                  nMCMC,
                                  clusters) {
  
  source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/saveNullDistributions.R")
  
  baseExposureId = as.numeric(substr(exposureId, 1, 5))
  
  dir1 = paste0("E:/Shounak_R/eumaeusTest_", 
                databaseId,
                "_Shounak/",
                methodName,
                "/e_",
                baseExposureId,
                "/estimates_t",
                tPeriod,
                ".csv")
  
  writeFolder = paste0("E:/Shounak_R/eumaeusTest_", 
                       databaseId,
                       "_Shounak/",
                       methodName,
                       "/e_",
                       baseExposureId,
                       "/nullFits/tPeriod=",
                       tPeriod,
                       "/analysisId=",
                       analysisId)
  
  estimatesFile <- read.csv(dir1)
  estimatesFile <- estimatesFile %>% filter(exposureId == !!exposureId)
  
  if(!(methodName %in% c("ConcurrentComparator", "ConcurrentComparator_1-28Days"))) {
    
    estimatesFile <- estimatesFile %>% filter(analysisId == !!analysisId)
    
  }
  
  settings <- list("subset" = estimatesFile,
                   "nMCMC" = nMCMC,
                   "writeFolder" = writeFolder,
                   "exposureId" = exposureId)
  
  allOutcomeIds <- unique(estimatesFile$outcomeId)
  
  ParallelLogger::clusterApply(clusters, allOutcomeIds, saveNullDistributionsClusterize, settings = settings)
  
}

