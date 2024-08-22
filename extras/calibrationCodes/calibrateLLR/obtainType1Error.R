

obtainType1Error <- function(periodEstimates, 
                             analysisId,
                             exposureId) {
  
  if("periodId" %in% colnames(periodEstimates)) {
    
    periodEstimates$seqId <- periodEstimates$periodId
    
  }
  
  signalMatrix <- data.frame("outcomeId" = unique(periodEstimates$outcomeId))
  maxTimePeriod <- max(periodEstimates$seqId)
  
  for(t in 1:maxTimePeriod) {
    
    signalMatrix[,paste0("time",t)] <- NA
    
  }
  
  signalMatrixUncalibrated <- signalMatrix
  
  relevantData <- periodEstimates %>% filter(analysisId == !!analysisId,
                                             exposureId == !!exposureId)
  
  for(i in 1:nrow(signalMatrix)) {
    
    for(j in 1:maxTimePeriod) {
      
      seqId <- j
      outcomeId <- signalMatrix$outcomeId[i]
      
      relevantIdx <- relevantData$outcomeId == outcomeId &
        relevantData$seqId == seqId
      
      if(sum(relevantIdx) == 1) {
        
        cv <- relevantData$criticalValue[relevantIdx]
        calibratedLlr <- relevantData$calibratedLlr[relevantIdx]
        uncalibratedLlr <- relevantData$llr[relevantIdx]
        
        if(!is.na(cv) & !is.na(calibratedLlr)) {
          
          signalMatrix[i,paste0("time",seqId)] <- ifelse(calibratedLlr >= cv, 1, 0)
          
        }
        
        if(!is.na(cv) & !is.na(uncalibratedLlr)) {
          
          signalMatrixUncalibrated[i,paste0("time",seqId)] <- ifelse(uncalibratedLlr >= cv, 1, 0)
          
        }
        
      }
      
      
    }
    
  }
  
  newSignalMatrix <- signalMatrix
  newSignalMatrixUncalibrated <- signalMatrixUncalibrated
  
  for(i in 1:nrow(newSignalMatrix)) {
    
    signalTimes <- which(newSignalMatrix[i,-1] == 1)
    signalTimesUncalibrated <- which(newSignalMatrixUncalibrated[i,-1] == 1)
    
    if(length(signalTimes) > 0) {
      
      firstSignalIdx <- min(signalTimes)
      
      for(j in firstSignalIdx:maxTimePeriod) {
        
        newSignalMatrix[i,paste0("time",j)] <- 1
        
      }
      
    }
    
    if(length(signalTimesUncalibrated) > 0) {
      
      firstSignalIdxUncalibrated <- min(signalTimesUncalibrated)
      
      for(j in firstSignalIdxUncalibrated:maxTimePeriod) {
        
        newSignalMatrixUncalibrated[i,paste0("time",j)] <- 1
        
      }
      
    }
    
  }
  
  output <- apply(newSignalMatrix[,-1], 2, mean, na.rm=T)
  output[is.nan(output)] <- NA
  
  outputUncalibrated <- apply(newSignalMatrixUncalibrated[,-1], 2, mean, na.rm=T)
  outputUncalibrated[is.nan(outputUncalibrated)] <- NA
  
  fullOutput <- as.data.frame(rbind(output, outputUncalibrated))
  fullOutput$Type <- c("Calibrated", "Uncalibrated")
  
  fullOutput$analysisId <- analysisId
  
  rownames(fullOutput) <- NULL
  
  return(fullOutput)
  
}

