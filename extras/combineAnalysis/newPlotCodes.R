#Calculate power and type 1 error on subset (NOT USING MAXSPRT) 
type1ErrorAndPower <- function(relevantData) {
  
  #fit leave-one-out null distributions
  nullModels <- list()
  calibratedCis <- matrix(0, nrow = nrow(relevantData), ncol = 2)
  colnames(calibratedCis) = c("calibratedCi95Lb", "calibratedCi95Ub")
  
  for(i in 1:nrow(relevantData)) {
    
    #fit the null
    nullModel = EmpiricalCalibration::fitNull(relevantData$logRr[-i],
                                              relevantData$seLogRr[-i])
    nullModels[[i]] = EmpiricalCalibration::convertNullToErrorModel(nullModel)
    
    #calibrate 95% CI
    calibratedCi = EmpiricalCalibration::calibrateConfidenceInterval(relevantData$logRr[i],
                                                                     relevantData$seLogRr[i],
                                                                     nullModels[[i]]) #log scale
    
    calibratedCis[i,] = exp(as.numeric(calibratedCi[c(2,3)]))
    
  }
  
  calibratedCis = as.data.frame(calibratedCis)
  
  #calculate type 1 error = proportion of calibrated CIs that do not contain 1
  
  type1Error = 1 - length(which((calibratedCis$calibratedCi95Lb <= 1) & (1 <= calibratedCis$calibratedCi95Ub))) / 
    nrow(calibratedCis)
  
  if(is.null(trueEffectSize)) {
    
    output = type1Error
    
  } else {
    
    #Now calculate type 2 error - uses trueEffectSize
    # #First calibrate using trueEffectSize
    # 
    # imputedNullModels <- list()
    # imputedCalibratedCis <- matrix(0, nrow = nrow(relevantData), ncol = 2)
    # colnames(imputedCalibratedCis) = c("imputedCalibratedCi95Lb", "imputedCalibratedCi95Ub")
    # 
    # for(i in 1:nrow(relevantData)) {
    #   
    #   #fit the null
    #   imputedNullModel = EmpiricalCalibration::fitNull(relevantData$logRr[-i] + log(trueEffectSize),
    #                                             relevantData$seLogRr[-i])
    #   imputedNullModels[[i]] = EmpiricalCalibration::convertNullToErrorModel(imputedNullModel)
    #   
    #   #calibrate 95% CI
    #   imputedCalibratedCi = EmpiricalCalibration::calibrateConfidenceInterval(relevantData$logRr[i] + log(trueEffectSize),
    #                                                                    relevantData$seLogRr[i],
    #                                                                    imputedNullModels[[i]]) #log scale
    #   
    #   imputedCalibratedCis[i,] = exp(as.numeric(imputedCalibratedCi[c(2,3)]))
    #   
    # }
    
    imputedCalibratedCis = trueEffectSize * calibratedCis
    colnames(imputedCalibratedCis) = c("imputedCalibratedCi95Lb", "imputedCalibratedCi95Ub")
    
    imputedCalibratedCis = as.data.frame(imputedCalibratedCis)
    
    #type 2 error = proportion of imputed calibrated CIs that CONTAIN 1
    
    type2Error = length(which((imputedCalibratedCis$imputedCalibratedCi95Lb <= 1) & 
                                (1 <= imputedCalibratedCis$imputedCalibratedCi95Ub))) / 
      nrow(calibratedCis)
    
    powerMethod = 1 - type2Error
    
    output = c(type1Error, powerMethod)
    
  }
  
  return(output)
  
}




############# ----------------------- Plots below are without MaxSPRT adjustment. ------------############
#function to plot type 1 error and power across time
plotType1ErrorAndPowerAcrossTime <- function(databaseId,
                                             exposureId,
                                             trueEffectSize,
                                             allEstimates,
                                             analysisIds,
                                             maxTimePeriod,
                                             exposureName) { #maxTimePeriod to be input by user for now
  
  negativeControlIds = read.csv("E:/Shounak_R/Eumaeus/inst/settings/NegativeControls.csv")
  ncIds = negativeControlIds$outcomeId
  
  #maxTimePeriod = max(unique((allEstimates %>% filter(exposureId == exposureId))$periodId))
  methodNames = c("ConcurrentComparator_1-28Days", "HistoricalComparator", "CohortMethod", "CaseControl", "SCCS")
  analysisIds = c(1, #dummy analysisId for concurrent comparator
                  analysisIds$HistoricalComparatorAnalysisId,
                  analysisIds$CohortMethodId,
                  analysisIds$CaseControlId,
                  analysisIds$SCCSAnalysisId)
  
  outputDf = NULL
  
  for(i in 1:length(methodNames)) {
    
    method = methodNames[i]
    subType1ErrorDf = rep(0, maxTimePeriod)
    subPowerDf = rep(0, maxTimePeriod)
    
    for(periodId in 1:maxTimePeriod) {
      
      type1ErrorAndPowerValues = type1ErrorAndPowerMethodWise(databaseId,
                                                              exposureId,
                                                              method,
                                                              analysisId = analysisIds[i],
                                                              periodId,
                                                              trueEffectSize = NULL,
                                                              ncIds,
                                                              allEstimates)
      
      subType1ErrorDf[periodId] = type1ErrorAndPowerValues[1]
      subPowerDf[periodId] = type1ErrorAndPowerValues[2]
      
    }
    
    subOutputDf = data.frame("Type1Error" = subType1ErrorDf, "Power" = subPowerDf, "Method" = method)
    outputDf = dplyr::bind_rows(outputDf, subOutputDf)
    
  }
  
  outputDf$Months = rep(1:maxTimePeriod, length(methodNames))
  
  type1ErrorPlot <- ggplot(outputDf, aes(x = Months, y = Type1Error, group = Method)) + 
    geom_line(aes(color=Method), linewidth = 1.5) + geom_point(size = 2) + theme_minimal() +
    scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")) +
    ylab("Type 1 Error") +
    ggtitle(paste0(exposureName, ", ", databaseId)) + scale_x_continuous(breaks = 1:maxTimePeriod) +
    theme(text = element_text(size=22))
  
  print(type1ErrorPlot)
  
  powerPlot <- ggplot(outputDf, aes(x = Months, y = Power, group = Method)) + 
    geom_line(aes(color = Method), linewidth = 1.5) + geom_point(size = 2) + theme_minimal() +
    scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")) +
    ylab("Power") +
    ggtitle(paste0(exposureName, ", ", databaseId, ", trueEffectSize = ", trueEffectSize)) + scale_x_continuous(breaks = 1:maxTimePeriod) +
    theme(text = element_text(size=22))
  
  print(powerPlot)
  
  output = list("Type1ErrorPlot" = type1ErrorPlot,
                "PowerPlot" = powerPlot)
  
}


#### Function to return type 1 and type 2 errors

# databaseId = "OptumEhr"
# exposureId = 21184 #H1n1, compare with 21215 (Flu)
# method = "HistoricalComparator"
# analysisId = 14
# periodId = 9
# trueEffectSize = 2
# 
# negativeControlIds = read.csv("E:/Shounak_R/Eumaeus/inst/settings/NegativeControls.csv")
# ncIds = negativeControlIds$outcomeId

#big error concerning baseexposureid when using concurrent comparator (probably resolved, double check)
type1ErrorAndPowerMethodWise <- function(databaseId,
                                         exposureId,
                                         method,
                                         analysisId = NULL,
                                         periodId,
                                         trueEffectSize = NULL,
                                         ncIds,
                                         allEstimates) {
  
  matchDataSourceName = rbind(c("IBM_MDCR", "truven_mdcr"),
                              c("IBM_MDCD", "truven_mdcd"),
                              c("CCAE", "truven_ccae"),
                              c("OptumEhr", "optum_ehr"),
                              c("OptumDod", "optum_extended_dod"))
  
  if(method == "ConcurrentComparator") {
    
    #locally obtain concurrent comparator summary file
    
    ccDatabaseId = matchDataSourceName[which(matchDataSourceName[,1] == databaseId),2]
    
    baseExposureId = as.numeric(substr(exposureId, 1, 5))
    
    file.dir = paste0("E:/eumaeusTest_", ccDatabaseId, "_Shounak/ConcurrentComparator/e_", baseExposureId, "/estimates_t", periodId, ".csv")
    ccRelevantData = read.csv(file.dir, header=TRUE)
    
    #only select the rows with the correct exposureId
    
    ccRelevantData = ccRelevantData %>% dplyr::filter(exposureId == !!exposureId)
    
    #Remove all rows with abs(logRr) > 10 and containing any NAs
    selectedIndices = which(rowSums(is.na(cbind(ccRelevantData$logRr,
                                                ccRelevantData$seLogRr,
                                                ccRelevantData$logLb95,
                                                ccRelevantData$logUb95))) == 0)
    ccRelevantData = ccRelevantData[selectedIndices,]
    
    selectedIndicesStable = which(abs(ccRelevantData$logRr) <= 5)
    ccRelevantData = ccRelevantData[selectedIndicesStable,]
    
    if(nrow(ccRelevantData) <= 2) {
      
      return(c(NA, NA))
      
    } else {
      
      return(type1ErrorAndPower(ccRelevantData))
      
    }
    
    
  } else {
    
    #filter 93 * ncols information on negative controls
    relevantData <- allEstimates %>% dplyr::filter(databaseId == !!databaseId,
                                                   exposureId == !!exposureId,
                                                   method == !!method,
                                                   analysisId == !!analysisId,
                                                   periodId == !!periodId,
                                                   outcomeId %in% ncIds)
    
    #remove any rows containing NA
    selectedIndices = which(rowSums(is.na(cbind(relevantData$logRr,
                                                relevantData$seLogRr,
                                                relevantData$ci95Lb,
                                                relevantData$ci95Ub))) == 0)
    relevantData = relevantData[selectedIndices,]
    
    if(nrow(relevantData) <= 5) {
      
      return(c(NA, NA))
      
    } else {
      
      return(type1ErrorAndPower(relevantData))
      
    }
    
  }
  
}

type1ErrorAndPower <- function(relevantData) {
  
  #fit leave-one-out null distributions
  nullModels <- list()
  calibratedCis <- matrix(0, nrow = nrow(relevantData), ncol = 2)
  colnames(calibratedCis) = c("calibratedCi95Lb", "calibratedCi95Ub")
  
  for(i in 1:nrow(relevantData)) {
    
    #fit the null
    nullModel = EmpiricalCalibration::fitNull(relevantData$logRr[-i],
                                              relevantData$seLogRr[-i])
    nullModels[[i]] = EmpiricalCalibration::convertNullToErrorModel(nullModel)
    
    #calibrate 95% CI
    calibratedCi = EmpiricalCalibration::calibrateConfidenceInterval(relevantData$logRr[i],
                                                                     relevantData$seLogRr[i],
                                                                     nullModels[[i]]) #log scale
    
    calibratedCis[i,] = exp(as.numeric(calibratedCi[c(2,3)]))
    
  }
  
  calibratedCis = as.data.frame(calibratedCis)
  
  #calculate type 1 error = proportion of calibrated CIs that do not contain 1
  
  type1Error = 1 - length(which((calibratedCis$calibratedCi95Lb <= 1) & (1 <= calibratedCis$calibratedCi95Ub))) / 
    nrow(calibratedCis)
  
  if(is.null(trueEffectSize)) {
    
    output = type1Error
    
  } else {
    
    #Now calculate type 2 error - uses trueEffectSize
    # #First calibrate using trueEffectSize
    # 
    # imputedNullModels <- list()
    # imputedCalibratedCis <- matrix(0, nrow = nrow(relevantData), ncol = 2)
    # colnames(imputedCalibratedCis) = c("imputedCalibratedCi95Lb", "imputedCalibratedCi95Ub")
    # 
    # for(i in 1:nrow(relevantData)) {
    #   
    #   #fit the null
    #   imputedNullModel = EmpiricalCalibration::fitNull(relevantData$logRr[-i] + log(trueEffectSize),
    #                                             relevantData$seLogRr[-i])
    #   imputedNullModels[[i]] = EmpiricalCalibration::convertNullToErrorModel(imputedNullModel)
    #   
    #   #calibrate 95% CI
    #   imputedCalibratedCi = EmpiricalCalibration::calibrateConfidenceInterval(relevantData$logRr[i] + log(trueEffectSize),
    #                                                                    relevantData$seLogRr[i],
    #                                                                    imputedNullModels[[i]]) #log scale
    #   
    #   imputedCalibratedCis[i,] = exp(as.numeric(imputedCalibratedCi[c(2,3)]))
    #   
    # }
    
    imputedCalibratedCis = trueEffectSize * calibratedCis
    colnames(imputedCalibratedCis) = c("imputedCalibratedCi95Lb", "imputedCalibratedCi95Ub")
    
    imputedCalibratedCis = as.data.frame(imputedCalibratedCis)
    
    #type 2 error = proportion of imputed calibrated CIs that CONTAIN 1
    
    type2Error = length(which((imputedCalibratedCis$imputedCalibratedCi95Lb <= 1) & 
                                (1 <= imputedCalibratedCis$imputedCalibratedCi95Ub))) / 
      nrow(calibratedCis)
    
    powerMethod = 1 - type2Error
    
    output = c(type1Error, powerMethod)
    
  }
  
  return(output)
  
}

#type 1 error and power for methods, for files stored locally (covid exposures)
#code for results from covid * old methods and covid * concurrent comparator
type1ErrorAndPowerMethodWiseCovid <- function(databaseId,
                                              exposureId,
                                              method,
                                              analysisId = NULL,
                                              periodId,
                                              trueEffectSize = NULL,
                                              ncIds) {
  
  matchDataSourceName = rbind(c("IBM_MDCR", "truven_mdcr"),
                              c("IBM_MDCD", "truven_mdcd"),
                              c("CCAE", "truven_ccae"),
                              c("OptumEhr", "optum_ehr"),
                              c("OptumDod", "optum_extended_dod"))
  
  #locally obtain method specific summary file
  
  methodDatabaseId = matchDataSourceName[which(matchDataSourceName[,1] == databaseId),2]
  
  baseExposureId = as.numeric(substr(exposureId, 1, 5))
  
  file.dir = paste0("E:/eumaeusTest_", methodDatabaseId, "_Shounak/", method, "/e_", baseExposureId, "/estimates_t", periodId, ".csv")
  methodRelevantData = read.csv(file.dir, header=TRUE)
  
  #only select the rows with the correct exposureId
  
  if(method == "ConcurrentComparator") {
    
    methodRelevantData = methodRelevantData %>% dplyr::filter(exposureId == !!exposureId)
    
    
  } else {
    
    methodRelevantData = methodRelevantData %>% dplyr::filter(exposureId == !!exposureId, 
                                                              analysisId == !!analysisId)
    
  }
  
  
  #Remove all rows with abs(logRr) > 5 and containing any NAs
  selectedIndices = which(rowSums(is.na(cbind(methodRelevantData$logRr,
                                              methodRelevantData$seLogRr))) == 0)
  methodRelevantData = methodRelevantData[selectedIndices,]
  
  selectedIndicesStable = which(abs(methodRelevantData$logRr) <= 5)
  methodRelevantData = methodRelevantData[selectedIndicesStable,]
  
  if(nrow(methodRelevantData) <= 2) {
    
    return(c(NA, NA))
    
  } else {
    
    return(type1ErrorAndPower(methodRelevantData))
    
  }
  
  #code for results from old exposures * old methods and old exposures * concurrent comparator
  # if(method == "ConcurrentComparator") {
  #   
  #   #locally obtain concurrent comparator summary file
  #   
  #   ccDatabaseId = matchDataSourceName[which(matchDataSourceName[,1] == databaseId),2]
  #   
  #   baseExposureId = as.numeric(substr(exposureId, 1, 5))
  #   
  #   file.dir = paste0("E:/eumaeusTest_", ccDatabaseId, "_Shounak/ConcurrentComparator/e_", baseExposureId, "/estimates_t", periodId, ".csv")
  #   ccRelevantData = read.csv(file.dir, header=TRUE)
  #   
  #   #only select the rows with the correct exposureId
  #   
  #   ccRelevantData = ccRelevantData %>% dplyr::filter(exposureId == !!exposureId)
  #   
  #   #Remove all rows with abs(logRr) > 10 and containing any NAs
  #   selectedIndices = which(rowSums(is.na(cbind(ccRelevantData$logRr,
  #                                               ccRelevantData$seLogRr,
  #                                               ccRelevantData$logLb95,
  #                                               ccRelevantData$logUb95))) == 0)
  #   ccRelevantData = ccRelevantData[selectedIndices,]
  #   
  #   selectedIndicesStable = which(abs(ccRelevantData$logRr) <= 5)
  #   ccRelevantData = ccRelevantData[selectedIndicesStable,]
  #   
  #   if(nrow(ccRelevantData) <= 2) {
  #     
  #     return(c(NA, NA))
  #     
  #   } else {
  #     
  #     return(type1ErrorAndPower(ccRelevantData))
  #     
  #   }
  #   
  #   
  # } else {
  #   
  #   #filter 93 * ncols information on negative controls
  #   relevantData <- allEstimates %>% dplyr::filter(databaseId == !!databaseId,
  #                                                  exposureId == !!exposureId,
  #                                                  method == !!method,
  #                                                  analysisId == !!analysisId,
  #                                                  periodId == !!periodId,
  #                                                  outcomeId %in% ncIds)
  #   
  #   #remove any rows containing NA
  #   selectedIndices = which(rowSums(is.na(cbind(relevantData$logRr,
  #                                               relevantData$seLogRr,
  #                                               relevantData$ci95Lb,
  #                                               relevantData$ci95Ub))) == 0)
  #   relevantData = relevantData[selectedIndices,]
  #   
  #   if(nrow(relevantData) <= 2) {
  #     
  #     return(c(NA, NA))
  #     
  #   } else {
  #     
  #     return(type1ErrorAndPower(relevantData))
  #     
  #   }
  #   
  # }
  
}

#plot function for covid * all methods
plotType1ErrorAndPowerAcrossTimeCovid <- function(databaseId,
                                                  exposureId,
                                                  trueEffectSize,
                                                  analysisIds,
                                                  maxTimePeriod,
                                                  exposureName) { #maxTimePeriod to be input by user for now
  
  negativeControlIds = read.csv("E:/Shounak_R/Eumaeus/inst/settings/NegativeControls.csv")
  ncIds = negativeControlIds$outcomeId
  
  #maxTimePeriod = max(unique((allEstimates %>% filter(exposureId == exposureId))$periodId))
  methodNames = c("ConcurrentComparator", "HistoricalComparator", "CaseControl", "SCCS")
  methodNamesDirectory = c("ConcurrentComparator", "historicalComparator", "caseControl", "sccs")
  analysisIds = c(1, #dummy analysisId for concurrent comparator
                  analysisIds$HistoricalComparatorAnalysisId,
                  analysisIds$CaseControlId,
                  analysisIds$SCCSAnalysisId)
  
  outputDf = NULL
  
  for(i in 1:length(methodNames)) {
    
    method = methodNamesDirectory[i]
    subType1ErrorDf = rep(0, maxTimePeriod)
    subPowerDf = rep(0, maxTimePeriod)
    
    for(periodId in 1:maxTimePeriod) {
      
      type1ErrorAndPowerValues = type1ErrorAndPowerMethodWiseCovid(databaseId,
                                                                   exposureId,
                                                                   method,
                                                                   analysisId = analysisIds[i],
                                                                   periodId,
                                                                   trueEffectSize = NULL,
                                                                   ncIds)
      
      subType1ErrorDf[periodId] = type1ErrorAndPowerValues[1]
      subPowerDf[periodId] = type1ErrorAndPowerValues[2]
      
    }
    
    subOutputDf = data.frame("Type1Error" = subType1ErrorDf, "Power" = subPowerDf, "Method" = method)
    outputDf = dplyr::bind_rows(outputDf, subOutputDf)
    
  }
  
  outputDf$Months = rep(1:maxTimePeriod, length(methodNames))
  
  type1ErrorPlot <- ggplot(outputDf, aes(x = Months, y = Type1Error, group = Method)) + 
    geom_line(aes(color=Method), linewidth = 1.5) + geom_point(size = 2) + theme_minimal() +
    scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")) +
    ylab("Type 1 Error") +
    ggtitle(paste0(exposureName, ", ", databaseId)) + scale_x_continuous(breaks = 1:maxTimePeriod) +
    theme(text = element_text(size=22))
  
  print(type1ErrorPlot)
  
  powerPlot <- ggplot(outputDf, aes(x = Months, y = Power, group = Method)) + 
    geom_line(aes(color = Method), linewidth = 1.5) + geom_point(size = 2) + theme_minimal() +
    scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")) +
    ylab("Power") +
    ggtitle(paste0(exposureName, ", ", databaseId, ", trueEffectSize = ", trueEffectSize)) + scale_x_continuous(breaks = 1:maxTimePeriod) +
    theme(text = element_text(size=22))
  
  print(powerPlot)
  
  output = list("Type1ErrorPlot" = type1ErrorPlot,
                "PowerPlot" = powerPlot)
  
}

#plot systematic error distributions across methods and exposures
