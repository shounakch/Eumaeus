### Obtain calibrated critical values for methods in covid exposures ###

# ConcurrentComparatorEstimates <- list()
# for(t in 1:7) {
#   
#   file.name = paste0("E:/eumaeusTest_optum_ehr_Shounak/ConcurrentComparator/e_21216/estimates_t", t, ".csv")
#   
#   ConcurrentComparatorEstimatesPeriod <- readr::read_csv(file = file.name)
#   ConcurrentComparatorEstimatesPeriod$seqId <- t
#   ConcurrentComparatorEstimates[[t]] <- ConcurrentComparatorEstimatesPeriod
#   
# }
# 
# ConcurrentComparatorEstimates <- bind_rows(ConcurrentComparatorEstimates) 
# ConcurrentComparatorEstimates <- ConcurrentComparatorEstimates %>%
#   mutate(approxLlr = ifelse(logRr <= 0, 0, dnorm(logRr, logRr, seLogRr) - dnorm(0, logRr, seLogRr)))
# 
# subset <- ConcurrentComparatorEstimates %>% filter(outcomeId == ConcurrentComparatorEstimates$outcomeId[5])
# 
# negativeControlIds = read.csv("E:/Shounak_R/Eumaeus/inst/settings/NegativeControls.csv")
# ncIds = negativeControlIds$outcomeId

# obtain type 1 error and power across negative control outcomes with MaxSPRT adjustment
# first consider old (non-covid) exposures

## Read allEstimates file from locally saved version

# allEstimates <- data.table::fread(input = "E:/Shounak_R/EumaeusAnalysis/allPastEstimates.csv",
#                                   sep = ",")
# allEstimatesImputedPcs <- data.table::fread(input = "E:/Shounak_R/EumaeusAnalysis/allPastEstimatesImputedPcs.csv",
#                                             sep = ",")
# allEstimatesImputedPcsDescription <- data.table::fread(input = "E:/Shounak_R/EumaeusAnalysis/allPastImputedPcAnalysesDescription.csv",
#                                                        sep = ",")

############ ------ PLOT CODES -------#############

#plots type 1 error and power across time for all methods, old exposures, using MaxSPRT
plotType1ErrorAndPowerAcrossTime <- function(databaseId,
                                             exposureId,
                                             trueEffectSize,
                                             analysisIds,
                                             maxTimePeriod,
                                             exposureName) { #maxTimePeriod to be input by user for now
  
  # negativeControlIds = read.csv("E:/Shounak_R/Eumaeus/inst/settings/NegativeControls.csv")
  # ncIds = negativeControlIds$outcomeId
  
  #maxTimePeriod = max(unique((allEstimates %>% filter(exposureId == exposureId))$periodId))
  methodNames = c("ConcurrentComparator", "HistoricalComparator", "CaseControl", "SCCS")
  
  analysisIds = c(1, #dummy analysisId for concurrent comparator
                  analysisIds$HistoricalComparatorAnalysisId,
                  analysisIds$CaseControlId,
                  analysisIds$SCCSAnalysisId)
  
  outputDf = NULL
  
  for(i in 1:length(methodNames)) {
    
    method = methodNames[i]
    
    # subType1ErrorDf = rep(0, maxTimePeriod)
    # subPowerDf = rep(0, maxTimePeriod)
    # 
    # for(periodId in 1:maxTimePeriod) {
    #   
    #   type1ErrorAndPowerValues = type1ErrorAndPowerMethodWise(databaseId,
    #                                                           exposureId,
    #                                                           method,
    #                                                           analysisId = analysisIds[i],
    #                                                           periodId,
    #                                                           trueEffectSize = NULL,
    #                                                           ncIds,
    #                                                           allEstimates)
    #   
    #   subType1ErrorDf[periodId] = type1ErrorAndPowerValues[1]
    #   subPowerDf[periodId] = type1ErrorAndPowerValues[2]
    #   
    # }
    
    type1ErrorAndPowerValues = type1ErrorPowerOldExposuresMaxSPRT(maxTimePeriod = maxTimePeriod,
                                                                  databaseId = databaseId,
                                                                  methodName = method,
                                                                  exposureId = exposureId,
                                                                  analysisId = analysisIds[i],
                                                                  trueEffectSize = trueEffectSize)
    
    subType1ErrorDf = type1ErrorAndPowerValues$type1Errors
    subPowerDf = type1ErrorAndPowerValues$powers
    
    subOutputDf = data.frame("Type1Error" = subType1ErrorDf, "Power" = subPowerDf, "Method" = method)
    outputDf = dplyr::bind_rows(outputDf, subOutputDf)
    
  }
  
  outputDf$Months = rep(1:maxTimePeriod, length(methodNames))
  
  type1ErrorPlot <- ggplot(outputDf, aes(x = Months, y = Type1Error, group = Method)) + 
    geom_line(aes(color=Method), linewidth = 1.5) + geom_point(size = 2) + theme_minimal() +
    scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")[-2]) +
    ylab("Type 1 Error") +
    ggtitle(paste0(exposureName, ", ", databaseId)) + scale_x_continuous(breaks = 1:maxTimePeriod) +
    theme(text = element_text(size=22))
  
  print(type1ErrorPlot)
  
  powerPlot <- ggplot(outputDf, aes(x = Months, y = Power, group = Method)) + 
    geom_line(aes(color = Method), linewidth = 1.5) + geom_point(size = 2) + theme_minimal() +
    scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")[-2]) +
    ylab("Power") +
    ggtitle(paste0(exposureName, ", ", databaseId, ", trueEffectSize = ", trueEffectSize)) + scale_x_continuous(breaks = 1:maxTimePeriod) +
    theme(text = element_text(size=22))
  
  print(powerPlot)
  
  output = list("Type1ErrorPlot" = type1ErrorPlot,
                "PowerPlot" = powerPlot)
  
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
                                               trueEffectSize) {
  
  library(dplyr)
  
  matchDataSourceName = rbind(c("IBM_MDCR", "truven_mdcr"),
                              c("IBM_MDCD", "truven_mdcd"),
                              c("CCAE", "truven_ccae"),
                              c("OptumEhr", "optum_ehr"),
                              c("OptumDod", "optum_extended_dod"))
  methodNamesDirectory = rbind(c("ConcurrentComparator", "ConcurrentComparator"),
                               c("HistoricalComparator", "historicalComparator"), 
                               c("CaseControl", "caseControl"),
                               c("SCCS", "sccs"))
  
  negativeControlIds = read.csv("E:/Shounak_R/Eumaeus/inst/settings/NegativeControls.csv")
  outcomeIds = negativeControlIds$outcomeId
  
  #if(methodName == "ConcurrentComparator") {
  
  localDatabaseId = matchDataSourceName[which(matchDataSourceName[,1] == databaseId), 2]
  
  baseExposureId = as.numeric(substr(exposureId, 1, 5))
  
  methodNameDir = methodNamesDirectory[which(methodNamesDirectory[,1] == methodName), 2]
  
  methodEstimates <- list()
  
  for(t in 1:maxTimePeriod) {
    
    file.name = paste0("E:/eumaeusTest_", localDatabaseId, "_Shounak/", methodNameDir, "/e_", baseExposureId, "/estimates_t", t, ".csv")
    
    methodEstimatesPeriod <- readr::read_csv(file = file.name)
    
    methodEstimatesPeriod <- methodEstimatesPeriod %>% filter(exposureId == !!exposureId)
    
    if(methodName != "ConcurrentComparator") {
      
      methodEstimatesPeriod <- methodEstimatesPeriod %>% filter(analysisId == !!analysisId)
      
    }
    
    methodEstimatesPeriod$seqId <- t
    methodEstimates[[t]] <- methodEstimatesPeriod
    
  }
  
  methodEstimates <- dplyr::bind_rows(methodEstimates) 
  
  #mutate approximate llr for concurrent comparator. will do exact later
  
  if(methodName == "ConcurrentComparator") {
    
    methodEstimates <- methodEstimates %>%
      mutate(llr = ifelse(logRr <= 0, 0, dnorm(logRr, logRr, seLogRr, log=TRUE) - 
                            dnorm(0, logRr, seLogRr, log=TRUE)))
    
  }

  type1ErrorAndPower <- type1ErrorPowerSubsetOldExposureMaxSPRT(methodEstimates,
                                                                outcomeIds,
                                                                methodName,
                                                                trueEffectSize)
  
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
  
  for(seqId in 1:maxTimePeriod) {
    
    subsetData_t <- methodEstimates %>% filter(seqId == !!seqId) #data for time t = seqId
    observedIndices = (!is.na(subsetData_t$seLogRr)) & (abs(subsetData_t$logRr) <= 5) 
    
    calibratedCvs = rep(0, length(observedIndices))
    nullModels <- list()
    
    nullMeans = rep(0, length(observedIndices))
    nullSds = rep(0, length(observedIndices))
    
    for(i in 1:length(observedIndices)) {
      
      # Obtain null distribution for empirical calibration
      
      observedIndices_i = observedIndices #indices for which we have information, without i
      observedIndices_i[i] = FALSE
      
      if(sum(observedIndices_i) >= 3) {
        
        nullModel = EmpiricalCalibration::fitNull(subsetData_t$logRr[observedIndices_i],
                                                  subsetData_t$seLogRr[observedIndices_i])
        nullModels[[i]] = EmpiricalCalibration::convertNullToErrorModel(nullModel)
        nullMeans[i] = nullModel[[1]]
        nullSds[i] = nullModel[[2]]
        
        # Obtain calibrated critical values
        
        if(methodName != "HistoricalComparator") {
          
          if((mean(!is.na(groupSizes[[i]])) == 1 & !is.na(z[[i]])) & sum(groupSizes[[i]] < 0) == 0) {
            
            calibratedCvs[i] = EmpiricalCalibration::computeCvBinomial(groupSizes = groupSizes[[i]],
                                                                       z = z[[i]],
                                                                       nullMean = nullMeans[i],
                                                                       nullSd = nullSds[i], 
                                                                       sampleSize = 10^4)
            
          } else {
            
            calibratedCvs[i] = NA
            
          }
          
        } else {
          
          if(mean(!is.na(groupSizes[[i]])) == 1 & sum(groupSizes[[i]] < 0) == 0) {
            
            calibratedCvs[i] = EmpiricalCalibration::computeCvPoisson(groupSizes = groupSizes[[i]],
                                                                      nullMean = nullMeans[i],
                                                                      nullSd = nullSds[i],
                                                                      sampleSize = 10^4)
            
          } else {
            
            calibratedCvs[i] = NA
            
          }
          
        }
        
        
        
      } else {
        
        nullMeans[i] = NA
        nullSds[i] = NA
        calibratedCvs[i] = NA
        
      }
      
    }
    
    type1Error = mean(subsetData_t$llr[!is.na(subsetData_t$llr)] > 
                        calibratedCvs[!is.na(subsetData_t$llr)], na.rm=T)
    type1Errors[seqId] = type1Error
    
    # Now compute power
    
    imputedLogRr = subsetData_t$logRr + log(trueEffectSize)
    imputedSeLogRr = subsetData_t$seLogRr
    imputedLlr = rep(0, length(observedIndices))
    
    for(i in 1:length(observedIndices)) {
      
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
                "powers" = powers)
  
  return(output)
  
}

# provides groupSizes and z-value for calculating calibrated critical values
#subset stratified with exposure, time, outcomeId.
necessaryQuantities <- function(methodName, subset) {
  
  if(methodName == "CaseControl") {
    
    sampleSizeUpperLimit <- max(subset$cases, na.rm = TRUE)
    if (sampleSizeUpperLimit == 0) {
      cv <- NA
      exposedCases <- NA
    } else {
      exposedCases <- subset %>%
        arrange(.data$seqId) %>%
        pull(.data$exposedCases)
      looks <- length(exposedCases)
      if (looks > 1) {
        exposedCases[2:looks] <- exposedCases[2:looks] - exposedCases[1:(looks-1)]
        exposedCases <- exposedCases[exposedCases != 0]
      }
      
    }
    
    if(length(exposedCases) == 0) {
      
      groupSizes <- NA
      
    } else {
      
      groupSizes <- exposedCases
      
    }
    
    #groupSizes <- cases
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
    
    if(!is.null(expectedOutcomes)) {
      
      groupSizes <- expectedOutcomes
      
    } else {
      
      groupSizes <- NA
      
    }
    
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
