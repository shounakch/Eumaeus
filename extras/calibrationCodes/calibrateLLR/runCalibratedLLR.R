source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/calibrateLLR/calibrateLLR.R")

databaseId = "truven_mdcd"
#nMCMC = 10^5
maxCores = 12

# for non-covid exposures
methodNames = c("ConcurrentComparator_1-28Days")
analysisIdsList = list(c(3))
baseExposureIds = c(21183, 21184, 21185, 21198, 21214, 21215, 21216, 21217)
#seqIdsList = list(c(1:12), c(1:9), c(1:9), c(1:12), c(1:9), c(1:9), c(1:7), c(1:7))
outcomeIds = readr::read_csv(system.file("settings/NegativeControls.csv",
                                         package = "Eumaeus"))
outcomeIds = outcomeIds$outcomeId

# # for covid exposures
# methodNames = c("sccs", "caseControl", "historicalComparator")
# analysisIdsList = list(c(1:15), c(1:6), c(1:12))
# baseExposureIds = c(21216, 21217)
# # seqIdsList = list(c(1:7), c(1:7))
# outcomeIds = readr::read_csv(system.file("settings/NegativeControls.csv",
#                                          package = "Eumaeus"))
# outcomeIds = outcomeIds$outcomeId

## Run the code

clusters = ParallelLogger::makeCluster(maxCores)

ParallelLogger::clusterRequire(clusters, "EmpiricalCalibration")
ParallelLogger::clusterRequire(clusters, "dplyr")

for(i in 1:length(methodNames)) {
  
  methodName = methodNames[i]
  analysisIds = analysisIdsList[[i]]
  
  calibratedLLRStore(methodName,
                     analysisIds,
                     databaseId,
                     baseExposureIds,
                     outcomeIds,
                     clusters,
                     force=TRUE)
  
}

ParallelLogger::stopCluster(clusters)

##functions to calibrate LLR using saved null distribution

# methodName = "ConcurrentComparator_1-28Days"
# analysisIds = c(3)
# databaseId = "optum_ehr"
# baseExposureIds = c(21216)
# outcomeIds = readr::read_csv(system.file("settings/NegativeControls.csv",
#                                          package = "Eumaeus"))
# outcomeIds = outcomeIds$outcomeId
# seqIds = 1:7
# maxCores = 12
# 
# clusters = ParallelLogger::makeCluster(maxCores)
# 
# ParallelLogger::clusterRequire(clusters, "EmpiricalCalibration")
# ParallelLogger::clusterRequire(clusters, "dplyr")