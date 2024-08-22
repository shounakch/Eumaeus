library(dplyr)
library(readr)

source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/calibrateEstimatesFile.R")

dataName = "truven_mdcd" #existing files have some bizarre error

baseExposureIds = c(21183, 21184, 21185, 21198, 21214, 21215, 21216, 21217)
allTimePeriods = c(12, 9, 9, 12, 9, 9, 7, 7)
methodName <- "ConcurrentComparator_1-28Days"
analysisIds <- c(3)

# baseExposureIds = c(21216, 21217)
# allTimePeriods = c(7, 7)
# methodName <- "historicalComparator"
# analysisIds <- 1:12 #depends on methodName

maxCores <- 12

clusters <- ParallelLogger::makeCluster(maxCores)
ParallelLogger::clusterRequire(clusters, "EmpiricalCalibration")
ParallelLogger::clusterRequire(clusters, "dplyr")

force = TRUE

for(i in 1:length(baseExposureIds)) {
  
  baseExposureId <- baseExposureIds[i]
  timePeriods <- allTimePeriods[i]
  
  periodEstimates <- list()
  for(t in 1:timePeriods) {
    
    localFileName <- paste0("E:/Shounak_R/eumaeusTest_",
                            dataName,
                            "_Shounak/",
                            methodName,
                            "/e_",
                            baseExposureId,
                            "/estimates_t",
                            t, 
                            ".csv")
    
    localEstimates <- readr::read_csv(localFileName)
    localEstimates$seqId <- t
    
    periodEstimates[[t]] <- localEstimates
    
  }
  
  periodEstimates <- bind_rows(periodEstimates)
  
  ## Obtain critical values and save them
  #periodEstimates file simply has estimates for data source * baseExposure * all time periods for baseExposure.
  #contains data on all outcomeIds * all time periods * all analysis IDs.
  
  outcomeIds = readr::read_csv(system.file("settings/NegativeControls.csv", package = "Eumaeus"))$outcomeId
  #analysisIds = c(3) #only for non-CC analyses
  writeFolder = paste0("E:/Shounak_R/eumaeusTest_",
                       dataName,
                       "_Shounak/",
                       methodName,
                       "/e_",
                       baseExposureId)
  
  print(paste0("Saving critical values for exposure: ",
               baseExposureId,
               ", for time periods: ",
               timePeriods, 
               ", for method: ", 
               methodName))
  
  saveCvsClusterize <- function(analysisId, settings) {
    
    source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/calibrateEstimatesFile.R")
    
    periodEstimates = settings$periodEstimates
    methodName = settings$methodName
    outcomeIds = settings$outcomeIds
    writeFolder = settings$writeFolder
    force = settings$force
    
    return(saveCriticalValues(periodEstimates,
                              methodName,
                              outcomeIds,
                              analysisId,
                              writeFolder,
                              force))
    
  }
  
  settings = list("periodEstimates" = periodEstimates,
                  "methodName" = methodName,
                  "outcomeIds" = outcomeIds,
                  "writeFolder" = writeFolder,
                  "force" = force)
  
  ParallelLogger::clusterApply(clusters, analysisIds, saveCvsClusterize, settings = settings)
  
  periodEstimates <- list()
  
  for(analysisId in analysisIds) {
    
    file.name = paste0(writeFolder, "/periodEstimates", "_analysisId=", analysisId, "_WithCvs.csv")
    loadedDf <- readr::read_csv(file.name)
    loadedDf <- loadedDf[,-1]
    
    periodEstimates[[analysisId]] <- loadedDf
    
  }
  
  periodEstimates <- bind_rows(periodEstimates)
  
  periodFileName = paste0(writeFolder, "/periodEstimatesWithCvs.csv")
  write.csv(periodEstimates, periodFileName)
  
  # saveCriticalValues(periodEstimates,
  #                    methodName,
  #                    outcomeIds,
  #                    analysisIds,
  #                    writeFolder,
  #                    force=TRUE)
  
}

ParallelLogger::stopCluster(clusters)