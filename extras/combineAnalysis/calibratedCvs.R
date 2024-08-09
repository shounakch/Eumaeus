### Obtain calibrated critical values for methods ###

library(DatabaseConnector)
library(dplyr)
library(ggplot2)

############ ----------------- RELEVANT CODE ----------------- #######################

#plots type 1 error and power across time for all methods, old exposures, using MaxSPRT
plotType1ErrorAndPowerAcrossTime <- function(databaseId,
                                             exposureId,
                                             trueEffectSize,
                                             allEstimates = NULL,
                                             allEstimatesImputedPcs = NULL,
                                             allEstimatesImputedPcsDescription = NULL,
                                             analysisIds,
                                             maxTimePeriod,
                                             exposureName) { #maxTimePeriod to be input by user for now
  
  # negativeControlIds = read.csv("E:/Shounak_R/Eumaeus/inst/settings/NegativeControls.csv")
  # ncIds = negativeControlIds$outcomeId
  
  #maxTimePeriod = max(unique((allEstimates %>% filter(exposureId == exposureId))$periodId))
  #methodNames = c("ConcurrentComparator", "HistoricalComparator", "CohortMethod", "CaseControl", "SCCS")
  methodNames = c("ConcurrentComparator_1-28Days", "HistoricalComparator", "CohortMethod", "CaseControl", "SCCS")
  
  analysisIds = c(1, #dummy analysisId for concurrent comparator
                  analysisIds$HistoricalComparatorAnalysisId,
                  analysisIds$CohortMethodId,
                  analysisIds$CaseControlId,
                  analysisIds$SCCSAnalysisId) 
  
  savedObjectPath = paste0("E:/Shounak_R/EumaeusAnalysis/type1power_",
                           databaseId,
                           "_exposureId=",
                           exposureId,
                           "_trueEffect=",
                           trueEffectSize,
                           ".RData")
  
  if(!file.exists(savedObjectPath)) {
  #if(TRUE) {
    
    outputDf = NULL
    
    for(i in 1:length(methodNames)) {
      
      method = methodNames[i]
      
      type1ErrorAndPowerValues = type1ErrorPowerOldExposuresMaxSPRT(maxTimePeriod = maxTimePeriod,
                                                                    databaseId = databaseId,
                                                                    methodName = method,
                                                                    exposureId = exposureId,
                                                                    analysisId = analysisIds[i],
                                                                    trueEffectSize = trueEffectSize,
                                                                    allEstimates = allEstimates,
                                                                    allEstimatesImputedPcs = allEstimatesImputedPcs,
                                                                    allEstimatesImputedPcsDescription = allEstimatesImputedPcsDescription)
      
      subType1ErrorDf = type1ErrorAndPowerValues$type1Errors
      subPowerDf = type1ErrorAndPowerValues$powers
      subType1ErrorUncalibratedDf = type1ErrorAndPowerValues$type1ErrorsUncalibrated
      
      if(method == "ConcurrentComparator_1-28Days") {
        
        method = "ConcurrentComparator"
        
      }
      
      subOutputDf = data.frame("Type1Error" = subType1ErrorDf, 
                               "Power" = subPowerDf, 
                               "Type1ErrorUncalibrated" = subType1ErrorUncalibratedDf,
                               "Method" = method)
      outputDf = dplyr::bind_rows(outputDf, subOutputDf)
      
    }
    
    outputDf$Months = rep(1:maxTimePeriod, length(methodNames))
    
    saveRDS(outputDf, file = paste0("E:/Shounak_R/EumaeusAnalysis/type1power_",
                                    databaseId,
                                    "_exposureId=",
                                    exposureId,
                                    "_trueEffect=",
                                    trueEffectSize,
                                    ".RData"))
    
  } else {
    
    outputDf = readRDS(savedObjectPath)
    
  }
  
  type1ErrorPlot <- ggplot(outputDf, aes(x = Months, y = Type1Error, group = Method)) + 
    geom_line(aes(color=Method), linewidth = 3) + geom_point(size = 2)  +
    #geom_line(aes(x = Months, y = Type1ErrorUncalibrated, color=Method), linetype = "dotted", linewidth = 2) +
    geom_hline(aes(yintercept = 0.05), linewidth = 2, linetype = "dashed") +
    scale_y_continuous("Type 1 Error", breaks = c(0, 0.05, 0.1, 0.2), limits = c(0, 0.2)) +
    scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")) +
    ggtitle(paste0(exposureName, ", ", databaseId)) + scale_x_continuous(breaks = 1:maxTimePeriod) +
    theme_minimal() +
    theme(text = element_text(size=50),
          axis.text.y = element_text(margin = margin(r = 10)))
  
  print(type1ErrorPlot)
  
  type1ErrorUnCalibratedPlot <- ggplot(outputDf, aes(x = Months, y = Type1ErrorUncalibrated, group = Method)) + 
    geom_line(aes(color=Method), linewidth = 3) + geom_point(size = 2)  +
    geom_hline(aes(yintercept = 0.05), linewidth = 2, linetype = "dashed") +
    scale_y_continuous("Type 1 Error, Uncalibrated", breaks = round(c(0, 0.05, 0.1, 0.2, max(outputDf$Type1ErrorUncalibrated)), 2), limits = c(0, max(outputDf$Type1ErrorUncalibrated))) +
    scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")) +
    ggtitle(paste0(exposureName, ", ", databaseId)) + scale_x_continuous(breaks = 1:maxTimePeriod) +
    theme_minimal() +
    theme(text = element_text(size=50),
          axis.text.y = element_text(margin = margin(r = 10))) #using 36
  
  print(type1ErrorUnCalibratedPlot)
  
  powerPlot <- ggplot(outputDf, aes(x = Months, y = Power, group = Method)) + 
    geom_line(aes(color = Method), linewidth = 3) + 
    geom_point(size = 2) + 
    theme_minimal() +
    scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")) +
    scale_y_continuous(breaks = seq(0, 1, length.out = 5), limits = c(0,1)) +
    ggtitle(paste0(exposureName, ", ", databaseId, ", trueEffectSize = ", trueEffectSize)) + scale_x_continuous(breaks = 1:maxTimePeriod) +
    theme(text = element_text(size=50),
          axis.text.y = element_text(margin = margin(r = 10)))
  
  print(powerPlot)
  
  output = list("Type1ErrorPlot" = type1ErrorPlot,
                "PowerPlot" = powerPlot,
                "OutputDf" = outputDf)
  
  return(output)
  
}

# maxTimePeriod = 9
# databaseId = "OptumEhr"
# methodName = "ConcurrentComparator"
# exposureId = 21184
# trueEffectSize = 2
# analysisId = 2

#output type 1 error and power for old exposures using MaxSPRT
type1ErrorPowerOldExposuresMaxSPRT <- function(maxTimePeriod,
                                               databaseId,
                                               methodName,
                                               exposureId,
                                               analysisId,
                                               trueEffectSize,
                                               allEstimates,
                                               allEstimatesImputedPcs,
                                               allEstimatesImputedPcsDescription) {
  
  library(dplyr)
  
  matchDataSourceName = rbind(c("IBM_MDCR", "truven_mdcr"),
                              c("IBM_MDCD", "truven_mdcd"),
                              c("CCAE", "truven_ccae"),
                              c("OptumEhr", "optum_ehr"),
                              c("OptumDod", "optum_extended_dod"))
  
  negativeControlIds = read.csv("E:/Shounak_R/Eumaeus/inst/settings/NegativeControls.csv")
  outcomeIds = negativeControlIds$outcomeId
  
  if(methodName == "ConcurrentComparator") {
    
    ccDatabaseId = matchDataSourceName[which(matchDataSourceName[,1] == databaseId), 2]
    
    baseExposureId = as.numeric(substr(exposureId, 1, 5))
    
    methodEstimates <- list()
    
    for(t in 1:maxTimePeriod) {
      
      file.name = paste0("E:/Shounak_R/eumaeusTest_", ccDatabaseId, "_Shounak/ConcurrentComparator/e_", baseExposureId, "/estimates_t", t, ".csv")
      
      methodEstimatesPeriod <- readr::read_csv(file = file.name)
      methodEstimatesPeriod <- methodEstimatesPeriod %>% filter(exposureId == !!exposureId)
      methodEstimatesPeriod$seqId <- t
      methodEstimates[[t]] <- methodEstimatesPeriod
      
    }
    
    methodEstimates <- dplyr::bind_rows(methodEstimates) 
    
    #mutate approximate llr for concurrent comparator. will do exact later
    
    methodEstimates <- methodEstimates %>%
      mutate(llr = ifelse(logRr <= 0, 0, dnorm(logRr, logRr, seLogRr, log=TRUE) - 
                            dnorm(0, logRr, seLogRr, log=TRUE)))
    
    type1ErrorAndPower <- type1ErrorPowerSubsetOldExposureMaxSPRT(methodEstimates,
                                                                  outcomeIds,
                                                                  "ConcurrentComparator",
                                                                  trueEffectSize)
    
  } else if(methodName == "ConcurrentComparator_1-28Days") {
    
    ccDatabaseId = matchDataSourceName[which(matchDataSourceName[,1] == databaseId), 2]
    
    baseExposureId = as.numeric(substr(exposureId, 1, 5))
    
    methodEstimates <- list()
    
    for(t in 1:maxTimePeriod) {
      
      file.name = paste0("E:/Shounak_R/eumaeusTest_", ccDatabaseId, "_Shounak/ConcurrentComparator_1-28Days/e_", baseExposureId, "/estimates_t", t, ".csv")
      
      methodEstimatesPeriod <- readr::read_csv(file = file.name)
      methodEstimatesPeriod <- methodEstimatesPeriod %>% filter(exposureId == !!exposureId)
      methodEstimatesPeriod$seqId <- t
      methodEstimates[[t]] <- methodEstimatesPeriod
      
    }
    
    methodEstimates <- dplyr::bind_rows(methodEstimates) 
    
    #mutate approximate llr for concurrent comparator. will do exact later
    
    # methodEstimates <- methodEstimates %>%
    #   mutate(llr = ifelse(logRr <= 0, 0, dnorm(logRr, logRr, seLogRr, log=TRUE) - 
    #                         dnorm(0, logRr, seLogRr, log=TRUE)))
    
    type1ErrorAndPower <- type1ErrorPowerSubsetOldExposureMaxSPRT(methodEstimates,
                                                                  outcomeIds,
                                                                  "ConcurrentComparator",
                                                                  trueEffectSize)
    
  } else {
    
    methodEstimates <- allEstimates %>% dplyr::filter(databaseId == !!databaseId,
                                                      exposureId == !!exposureId,
                                                      method == !!methodName,
                                                      analysisId == !!analysisId,
                                                      outcomeId %in% outcomeIds)
    
    methodEstimatesImputedPcs <- allEstimatesImputedPcs %>% dplyr::filter(databaseId == !!databaseId,
                                                                          exposureId == !!exposureId,
                                                                          method == !!methodName,
                                                                          analysisId == !!analysisId,
                                                                          trueEffectSize == !!trueEffectSize)
    
    imputedPcOutcomeIds <- allEstimatesImputedPcsDescription$outcomeId[allEstimatesImputedPcsDescription$effectSize == trueEffectSize]
    
    type1Errors = rep(0, maxTimePeriod)
    powers = rep(0, maxTimePeriod)
    type1ErrorsUncalibrated = rep(0, maxTimePeriod)
    
    for(t in 1:maxTimePeriod) {
      
      subsetData_t <- methodEstimates %>% filter(periodId == !!t)
      
      availableIndices = !is.na(subsetData_t$calibratedLlr) &
        !is.na(subsetData_t$criticalValue)
      
      if(sum(availableIndices) > 3) {
        
        type1Error = mean(subsetData_t$calibratedLlr[availableIndices] >
                            subsetData_t$criticalValue[availableIndices])
        type1ErrorUncalibrated = mean(subsetData_t$llr[availableIndices] >
                                        subsetData_t$criticalValue[availableIndices])
        
        type1Errors[t] = type1Error
        type1ErrorsUncalibrated[t] = type1ErrorUncalibrated
        
      } else {
        
        type1Errors[t] = NA
        type1ErrorsUncalibrated[t] = NA
        
      }
      
      # Calculate power
      
      subsetData_t_imputedPc <- methodEstimatesImputedPcs %>% filter(periodId == !!t,
                                                                     outcomeId %in% imputedPcOutcomeIds)
      
      availableIndicesImputedPcs <- !is.na(subsetData_t_imputedPc$calibratedLlr) &
        !is.na(subsetData_t_imputedPc$criticalValue)
      
      if(sum(availableIndicesImputedPcs) > 3) {
        
        powerMethod = mean(subsetData_t_imputedPc$calibratedLlr[availableIndicesImputedPcs] >
                             subsetData_t_imputedPc$criticalValue[availableIndicesImputedPcs])
        
        powers[t] = powerMethod
        
      } else {
        
        powers[t] = NA
        
      }
      
    }
    
    type1ErrorAndPower <- list("type1Errors" = type1Errors,
                               "powers" = powers,
                               "type1ErrorsUncalibrated" = type1ErrorsUncalibrated)
    
  }
  
  return(type1ErrorAndPower)
  
}

#compute type 1 error and power on subset
#works on locally saved estimates file
type1ErrorPowerSubsetOldExposureMaxSPRT <- function(methodEstimates,
                                                    outcomeIds,
                                                    methodName,
                                                    trueEffectSize) {
  
  groupSizes <- list()
  z <- list()
  
  for(i in 1:length(outcomeIds)) {
    
    outcomeId = outcomeIds[i]
    
    subsetOutcome <- methodEstimates %>% filter(outcomeId == !!outcomeId)
    
    if(nrow(subsetOutcome) > 0) {
      
      necessaryQuantitiesOutcome <- necessaryQuantities(methodName, subsetOutcome)
      
      groupSizes[[i]] = necessaryQuantitiesOutcome$groupSizes
      z[[i]] = necessaryQuantitiesOutcome$z
      
    } else {
      
      groupSizes[[i]] = NA
      z[[i]] = NA
      
    }
    
  }
  
  type1Errors = rep(0, maxTimePeriod)
  powers = rep(0, maxTimePeriod)
  type1ErrorsUncalibrated = rep(0, maxTimePeriod)
  powersUncalibrated = rep(0, maxTimePeriod)
  
  for(seqId in 1:maxTimePeriod) {
    
    subsetData_t <- methodEstimates %>% filter(seqId == !!seqId) #data for time t = seqId
    
    calibratedCvs = rep(0, length(outcomeIds))
    uncalibratedCvs <- rep(0, length(outcomeIds))
    nullModels <- list()
    observedIndices = (!is.na(subsetData_t$seLogRr)) & (abs(subsetData_t$logRr) <= 5) 
    nullMeans = rep(0, length(outcomeIds))
    nullSds = rep(0, length(outcomeIds))
    
    for(i in 1:length(outcomeIds)) {
      
      # Obtain null distribution for empirical calibration
      
      observedIndices_i = observedIndices #indices for which we have information, without i
      observedIndices_i[i] = FALSE
      
      if(sum(observedIndices_i) >= 3) {
        
        nullModel = EmpiricalCalibration::fitNull(subsetData_t$logRr[observedIndices_i],
                                                  subsetData_t$seLogRr[observedIndices_i])
        #nullModels[[i]] = EmpiricalCalibration::convertNullToErrorModel(nullModel)
        #nullMeans[i] = nullModel[[1]]
        #nullSds[i] = nullModel[[2]]
        
        nullMeans[i] = nullModel[1]
        nullSds[i] = nullModel[2]
        
        # Obtain calibrated critical values
        
        if(mean(!is.na(groupSizes[[i]])) == 1 & !is.na(z[[i]])) {
          
          calibratedCvs[i] = EmpiricalCalibration::computeCvBinomial(groupSizes = groupSizes[[i]],
                                                                     z = z[[i]],
                                                                     nullMean = nullMeans[i],
                                                                     nullSd = nullSds[i], 
                                                                     sampleSize = 10^4)
          
          uncalibratedCvs[i] = EmpiricalCalibration::computeCvBinomial(groupSizes = groupSizes[[i]],
                                                                       z = z[[i]],
                                                                       nullMean = 0,
                                                                       nullSd = 0, 
                                                                       sampleSize = 10^4)
          
        } else {
          
          calibratedCvs[i] = NA
          uncalibratedCvs[i] = NA
          
        }
        
      } else {
        
        nullMeans[i] = NA
        nullSds[i] = NA
        calibratedCvs[i] = NA
        uncalibratedCvs[i] = NA
        
      }
      
    }
    
    type1Error = mean(subsetData_t$llr[!is.na(subsetData_t$llr)] > 
                        calibratedCvs[!is.na(subsetData_t$llr)],
                      na.rm=TRUE)
    type1Errors[seqId] = type1Error
    
    type1ErrorUncalibrated = mean(subsetData_t$llr[!is.na(subsetData_t$llr)] > 
                                    uncalibratedCvs[!is.na(subsetData_t$llr)],
                                  na.rm=TRUE)
    type1ErrorsUncalibrated[seqId] = type1ErrorUncalibrated
    
    # Now compute power
    
    imputedLogRr = subsetData_t$logRr + log(trueEffectSize)
    imputedSeLogRr = subsetData_t$seLogRr
    imputedLlr = rep(0, length(outcomeIds))
    
    for(i in 1:length(outcomeIds)) {
      
      if(!is.na(subsetData_t$seLogRr[i])) {
        
        if(imputedLogRr[i] <= 0) {
          
          imputedLlr[i] = 0
          
        }else {
          
          imputedLlr[i] = dnorm(imputedLogRr[i], imputedLogRr[i], imputedSeLogRr[i], log = TRUE) - 
            dnorm(0, imputedLogRr[i], imputedSeLogRr[i], log = TRUE)
          
        }
        
      } else {
        
        imputedLlr[i] = NA
        
      }
      
    }
    
    powerMethod = mean(imputedLlr[!is.na(imputedLlr)] > calibratedCvs[!is.na(imputedLlr)],
                       na.rm=TRUE)
    powers[seqId] = powerMethod
    
  }
  
  output = list("type1Errors" = type1Errors,
                "powers" = powers,
                "type1ErrorsUncalibrated" = type1ErrorsUncalibrated)
  
  return(output)
  
}

# provides groupSizes and z-value for calculating calibrated critical values
#subset stratified with exposure, time, outcomeId.
necessaryQuantities <- function(methodName, subset) {
  
  if(methodName == "CaseControl") {
    
    sampleSizeUpperLimit <- max(subset$cases, na.rm = TRUE)
    if (sampleSizeUpperLimit == 0) {
      cv <- NA
    } else {
      cases <- subset %>%
        arrange(.data$seqId) %>%
        pull(.data$cases)
      looks <- length(cases)
      if (looks > 1) {
        cases[2:looks] <- cases[2:looks] - cases[1:(looks-1)]
        cases <- cases[cases != 0]
      }
      
    }
    
    groupSizes <- cases
    z <- max(subset$controls, na.rm=T) / max(subset$cases, na.rm=T)
    
    output = list("groupSizes" = groupSizes,
                  "z" = z)
    
    return(output)
    
  } else if(methodName == "SCCS") {
    
    sampleSizeUpperLimit <- max(subset$outcomeEvents , na.rm = TRUE)
    if (sampleSizeUpperLimit == 0) {
      cv <- NA
    } else {
      events <- subset %>%
        arrange(.data$seqId) %>%
        pull(.data$outcomeEvents )
      looks <- length(events)
      if (looks > 1) {
        events[2:looks] <- events[2:looks] - events[1:(looks-1)]
        events <- events[events != 0]
      }
      
    }
    
    groupSizes <- events
    z <- max(subset$daysObserved - subset$exposedDays, na.rm=T) / max(subset$exposedDays, na.rm=T)
    
    output = list("groupSizes" = groupSizes,
                  "z" = z)
    
    return(output)
    
  } else if(methodName == "CohortMethod") {
    
    subset$events <- subset$eventsTarget + subset$eventsComparator
    sampleSizeUpperLimit <- max(subset$events, na.rm = TRUE)
    if (sampleSizeUpperLimit == 0) {
      cv <- NA
    } else {
      events <- subset %>%
        arrange(.data$seqId) %>%
        pull(.data$events)
      looks <- length(events)
      if (looks > 1) {
        events[2:looks] <- events[2:looks] - events[1:(looks-1)]
        events <- events[events > 0]
        sampleSizeUpperLimit <- sum(events)
      }
      
    }
    
    groupSizes <- events
    z <- max(subset$comparatorDays, na.rm=T) / max(subset$targetDays, na.rm=T)
    
    output = list("groupSizes" = groupSizes,
                  "z" = z)
    
    return(output)
    
  } else if(methodName == "HistoricalComparator") {
    
    expectedOutcomes <- subset %>%
      arrange(.data$seqId) %>%
      pull(.data$expectedOutcomes)
    looks <- length(expectedOutcomes)
    if (looks > 1) {
      expectedOutcomes[2:looks] <- expectedOutcomes[2:looks] - expectedOutcomes[1:(looks-1)]
      # Per-look expected counts < 1 can lead to CV.Poisson() getting stuck in infinite loop, so combining smaller looks:
      eos <- c()
      pending <- 0
      for (eo in expectedOutcomes) {
        if (!is.na(eo)) {
          if (eo + pending >= 1) {
            eos <- c(eos, eo + pending)
            pending <- 0
          } else {
            pending <- eo + pending
          }
        }
      }
      expectedOutcomes <- eos
      sampleSizeUpperLimit <- sum(expectedOutcomes)
    }
    
    groupSizes <- expectedOutcomes
    z <- NA
    
    output = list("groupSizes" = groupSizes,
                  "z" = z)
    
    return(output)
    
  } else if(methodName == "ConcurrentComparator") {
    
    subset$events <- subset$targetOutcomes + subset$comparatorOutcomes
    sampleSizeUpperLimit <- max(subset$events, na.rm = TRUE)
    if (sampleSizeUpperLimit == 0) {
      cv <- NA
      events <- NA
    } else {
      events <- subset %>%
        arrange(.data$seqId) %>%
        pull(.data$events)
      
      events[is.na(events) == T] = 0
      
      looks <- length(events)
      if (looks > 1) {
        events[2:looks] <- events[2:looks] - events[1:(looks-1)]
        events <- events[events > 0]
        sampleSizeUpperLimit <- sum(events)
        
      }
      
    }
    
    groupSizes <- events
    z <- max(subset$comparatorYears, na.rm=T) / max(subset$targetYears, na.rm=T)
    
    output = list("groupSizes" = groupSizes,
                  "z" = z)
    
    return(output)
    
  } else {
    
    print("Method not recognized!")
    
  }
  
}
