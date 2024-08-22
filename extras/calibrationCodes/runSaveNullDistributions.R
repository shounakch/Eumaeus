##For concurrent comparator
methodNames = "ConcurrentComparator_1-28Days"
exposureIds = c(211831, 211832, 211833, 21184, 21185, 211981, 211982, 211983, 21214, 21215, 21216, 21217)
maxTimePeriods = c(12, 12, 12, 9, 9, 12, 12, 12, 9, 9, 7, 7)
#exposureIds = c(21216, 21217)
#maxTimePeriods = c(7, 7)
analysisIdsList = list(c(3))

##For other methods
# methodNames = c("sccs", "caseControl", "historicalComparator")
# exposureIds = c(21216, 21217)
# maxTimePeriods = c(7, 7)
# analysisIdsList = list(c(1:15), c(1:6), c(1:12))

source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/saveNullDistributions.R")

databaseId = "truven_mdcd"
nMCMC = 10^5
maxCores = 12

clusters = ParallelLogger::makeCluster(maxCores)

ParallelLogger::clusterRequire(clusters, "EmpiricalCalibration")
ParallelLogger::clusterRequire(clusters, "dplyr")

for(idx in 1:length(methodNames)) {
  
  methodName = methodNames[idx]
  analysisIds = analysisIdsList[[idx]]
  
  # exposureIds = c(21216, 21217)
  # maxTimePeriods = c(7, 7)
  
  for(i in 1:length(exposureIds)) {
    
    exposureId = exposureIds[i]
    maxTimePeriod = maxTimePeriods[i]
    baseExposureId = as.numeric(substr(exposureId, 1, 5))
    
    baseNullFitsFolder <- paste0("E:/Shounak_R/eumaeusTest_",
                                 databaseId,
                                 "_Shounak/",
                                 methodName,
                                 "/e_",
                                 baseExposureId,
                                 "/nullFits")
    
    if(!file.exists(baseNullFitsFolder)) {dir.create(baseNullFitsFolder)}
    
    for(tPeriod in 1:maxTimePeriod) {
      
      baseTPeriodFolder <- paste0("E:/Shounak_R/eumaeusTest_",
                                  databaseId,
                                  "_Shounak/",
                                  methodName,
                                  "/e_",
                                  baseExposureId,
                                  "/nullFits/tPeriod=",
                                  tPeriod)
      
      if(!file.exists(baseTPeriodFolder)) {dir.create(baseTPeriodFolder)}
      
      for(analysisId in analysisIds) {
        
        print(paste0("Fitting null distributions for method: ",
                     methodName,
                     ", exposureId: ",
                     exposureId,
                     ", tPeriod: ",
                     tPeriod, 
                     ", analysisId: ",
                     analysisId))
        
        baseAnalysisIdFolder <- paste0("E:/Shounak_R/eumaeusTest_",
                                       databaseId,
                                       "_Shounak/",
                                       methodName,
                                       "/e_",
                                       baseExposureId,
                                       "/nullFits/tPeriod=",
                                       tPeriod,
                                       "/analysisId=",
                                       analysisId)
        
        if(!file.exists(baseAnalysisIdFolder)) {dir.create(baseAnalysisIdFolder)}
        
        saveNullDistributions(databaseId,
                              exposureId,
                              methodName,
                              analysisId,
                              tPeriod,
                              nMCMC,
                              clusters)   
        
      }
      
    }
    
  }
  
}

ParallelLogger::stopCluster(clusters)


