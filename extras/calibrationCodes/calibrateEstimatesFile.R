
saveCriticalValues <- function(periodEstimates,
                               methodName,
                               outcomeIds,
                               analysisIds,
                               writeFolder,
                               force = FALSE) {
  
  # analysisIdsFile <- system.file("settings/Analyses.csv", package = "Eumaeus")
  # analysisIds <- readr::read_csv(analysisIdsFile)
  # analysisIds <- analysisIds %>% filter()
  
  for(analysisId in analysisIds) {
    
    periodEstimatesFileName <- paste0(writeFolder, "/periodEstimates", "_analysisId=", analysisId, "_WithCvs.csv")
    
    if(!file.exists(periodEstimatesFileName) | force) {
      
      periodEstimates$criticalValue <- NA
      exposureIds <- unique(periodEstimates$exposureId)
      
      for(exposureId in exposureIds) {
        
        for(outcomeId in outcomeIds) {
          
          if(methodName %in% c("ConcurrentComparator", "ConcurrentComparator_1-28Days")) {
            
            outcomeExposureEstimates <- periodEstimates %>% filter(outcomeId == !!outcomeId,
                                                                   exposureId == !!exposureId)
            
          } else {
            
            outcomeExposureEstimates <- periodEstimates %>% filter(outcomeId == !!outcomeId,
                                                                   exposureId == !!exposureId,
                                                                   analysisId == !!analysisId)
            
          }
          
          if(nrow(outcomeExposureEstimates) > 0) {
            
            necessaryQuantitiesStore <- necessaryQuantities(methodName,
                                                            outcomeExposureEstimates)
            
            groupSizes <- necessaryQuantitiesStore$groupSizes
            z <- necessaryQuantitiesStore$z
            
            if(methodName == "historicalComparator") {
              
              if(mean(!is.na(groupSizes)) == 1 & sum(groupSizes < 0) == 0) {
                
                cv <- EmpiricalCalibration::computeCvPoisson(groupSizes = groupSizes)
                
              } else {
                
                cv <- NA
                
              }
              
            } else {
              
              if((mean(!is.na(groupSizes)) == 1 & !is.na(z)) & sum(groupSizes < 0) == 0) {
                
                cv <- EmpiricalCalibration::computeCvBinomial(groupSizes = groupSizes,
                                                              z = z)
                
              } else {
                
                cv <- NA
                
              }
              
            }
            
          } else {
            
            groupSizes <- NA
            z <- NA
            cv <- NA
            
          }
          
          if(methodName %in% c("ConcurrentComparator", "ConcurrentComparator_1-28Days")) {
            
            periodEstimates[(periodEstimates$outcomeId == outcomeId) & 
                              (periodEstimates$exposureId == exposureId),]$criticalValue <- cv
            
          } else {
            
            periodEstimates[(periodEstimates$outcomeId == outcomeId) & 
                              (periodEstimates$exposureId == exposureId) &
                              (periodEstimates$analysisId == analysisId),]$criticalValue <- cv
            
          }
          
        }
        
      }
      
      write.csv(periodEstimates[periodEstimates$analysisId == analysisId,], periodEstimatesFileName)
      
    } else {
      
      ParallelLogger::logInfo("File already exists!")
      
    }
    
  }
  
}


# Obtain necessary quantities to calculate critical values
necessaryQuantities <- function(methodName, subset) {
  
  if(methodName == "caseControl") {
    
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
    
  } else if(methodName == "sccs") {
    
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
    
  } else if(methodName == "cohortMethod") {
    
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
    
  } else if(methodName == "historicalComparator") {
    
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
    
  } else if(methodName %in% c("ConcurrentComparator", "ConcurrentComparator_1-28Days")) {
    
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
