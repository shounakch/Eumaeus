source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/calibrateLLR/obtainType1Error.R")
source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/calibrateLLR/obtainPower.R")

#methodName <- "ConcurrentComparator_1-28Days"
methodName <- "sccs"
analysisIds <- c(1:4,13)
dataName <- "optum_ehr"
exposureId <- 21216
baseExposureId <- as.numeric(substr(exposureId, 1, 5))
trueEffectSizes <- c(1.5, 2, 4)

periodEstimatesFileName <- paste0("E:/Shounak_R/eumaeusTest_",
                                  dataName,
                                  "_Shounak/",
                                  methodName,
                                  "/e_",
                                  baseExposureId,
                                  "/periodEstimatesWithCvsCalibrated.csv")

periodEstimates <- readr::read_csv(periodEstimatesFileName)

type1Error <- lapply(analysisIds, obtainType1Error, periodEstimates = periodEstimates,
                     exposureId = exposureId)
type1Error <- bind_rows(type1Error)

allPower <- lapply(analysisIds, obtainPower, 
                   periodEstimates = periodEstimates, 
                   exposureId = exposureId, 
                   trueEffectSizes = trueEffectSizes)
allPower <- bind_rows(allPower)

View(type1Error)
View(allPower)

#order (from in to out ) of loop: seqId, analysisId, exposureId, databaseId, method
