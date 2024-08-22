#### First obtain critical values ####

library(dplyr)
library(readr)

source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/calibrateEstimatesFile.R")

dataName = "optum_ehr"
timePeriods <- 12
baseExposureId <- 21183
methodName <- "ConcurrentComparator_1-28Days"

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
  
  localFileName <- paste0("E:/Shounak_R/eumaeusTest_optum_ehr_Shounak/ConcurrentComparator_1-28Days/e_21183/estimates_t", t, ".csv")
  localEstimates <- readr::read_csv(localFileName)
  localEstimates$seqId <- t
  
  periodEstimates[[t]] <- localEstimates
  
}

periodEstimates <- bind_rows(periodEstimates)

## Obtain critical values and save them
#periodEstimates file simply has estimates for data source * baseExposure * all time periods for baseExposure.
#contains data on all outcomeIds * all time periods * all analysis IDs.

outcomeIds = readr::read_csv(system.file("settings/NegativeControls.csv", package = "Eumaeus"))$outcomeId
analysisIds = c(3)
writeFolder = paste0("E:/Shounak_R/eumaeusTest_",
                     dataName,
                     "_Shounak/",
                     methodName,
                     "/e_",
                     baseExposureId)

saveCriticalValues(periodEstimates,
                   methodName,
                   outcomeIds,
                   analysisIds,
                   writeFolder)