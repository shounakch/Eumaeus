

obtainPower <- function(periodEstimates, 
                        analysisId,
                        exposureId,
                        trueEffectSizes) {
  
  if("periodId" %in% colnames(periodEstimates)) {
    
    periodEstimates$seqId <- periodEstimates$periodId
    
  }
  
  maxTimePeriod <- max(periodEstimates$seqId)
  
  relevantData <- periodEstimates %>% filter(analysisId == !!analysisId,
                                             exposureId == !!exposureId)
  
  allPowerStorage <- matrix(0, nrow = length(trueEffectSizes), ncol = 1 + maxTimePeriod + 1)
  colnames(allPowerStorage) <- c("trueEffectSize", paste0("time", 1:maxTimePeriod), "analysisId")
  allPowerStorage <- as.data.frame(allPowerStorage)
  
  allPowerStorage$analysisId <- analysisId
  
  for(trueEffectSize in trueEffectSizes) {
    
    signalMatrix <- data.frame("outcomeId" = unique(periodEstimates$outcomeId))
    for(t in 1:maxTimePeriod) {
      
      signalMatrix[,paste0("time",t)] <- NA
      
    }
    
    ## Mutate corresponding imputed calibrated Llrs
    
    allPowerStorage$trueEffectSize[which(trueEffectSizes == trueEffectSize)] <- trueEffectSize
    
    relevantDataDummy <- relevantData %>% mutate(imputedCalibratedLogRr = calibratedLogRr + log(trueEffectSize),
                                                 imputedCalibratedSeLogRr = calibratedSeLogRr)
    
    relevantDataDummy <- relevantDataDummy %>% mutate(imputedCalibratedLlr = ifelse(imputedCalibratedLogRr <= 0, 0,
    dnorm(imputedCalibratedLogRr, imputedCalibratedLogRr, imputedCalibratedSeLogRr, log=TRUE) - 
      dnorm(0, imputedCalibratedLogRr, imputedCalibratedSeLogRr, log=TRUE)))
    
    for(i in 1:nrow(signalMatrix)) {
      
      for(j in 1:maxTimePeriod) {
        
        seqId <- j
        outcomeId <- signalMatrix$outcomeId[i]
        
        relevantIdx <- relevantData$outcomeId == outcomeId &
          relevantData$seqId == seqId
        
        if(sum(relevantIdx) == 1) {
          
          cv <- relevantDataDummy$criticalValue[relevantIdx]
          calibratedLlr <- relevantDataDummy$imputedCalibratedLlr[relevantIdx]
          
          if(!is.na(cv) & !is.na(calibratedLlr)) {
            
            signalMatrix[i,paste0("time",seqId)] <- ifelse(calibratedLlr >= cv, 1, 0)
            
          }
          
        }
        
        
      }
      
    }
    
    newSignalMatrix <- signalMatrix
    
    for(i in 1:nrow(newSignalMatrix)) {
      
      signalTimes <- which(newSignalMatrix[i,-1] == 1)
      
      if(length(signalTimes) > 0) {
        
        firstSignalIdx <- min(signalTimes)
        
        for(j in firstSignalIdx:maxTimePeriod) {
          
          newSignalMatrix[i,paste0("time",j)] <- 1
          
        }
        
      }
      
    }
    
    output <- apply(newSignalMatrix[,-1], 2, mean, na.rm=T)
    output[is.nan(output)] <- NA
    
    # Save output
    
    allPowerStorage[which(trueEffectSizes == trueEffectSize),paste0("time",1:maxTimePeriod)] <- output
    
    
  }
  
  return(allPowerStorage)
  
}

