clusterFunction <- function(outcomeId, settings) {
  
  source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/calibrateLLR/calibrateLLR.R")
  
  periodEstimates <- settings$periodEstimates
  exposureId <- settings$exposureId
  analysisId <- settings$analysisId
  seqId <- settings$seqId
  methodName <- settings$methodName
  databaseId <- settings$databaseId
  exposureFolder <- settings$exposureFolder
  
  # relevantEstimate <- subsetEstimate %>% filter(exposureId == !!exposureId,
  #                                               analysisId == !!analysisId,
  #                                               seqId == !!seqId)
  
  idx <- periodEstimates$analysisId == analysisId & 
    periodEstimates$outcomeId == outcomeId &
    periodEstimates$seqId == seqId &
    periodEstimates$exposureId == exposureId #index to write the results
  
  if(sum(idx) == 1) {
    
    # Obtain LL Profile
    
    llProfile <- extractLLProfile(methodName,
                                  analysisId,
                                  databaseId,
                                  exposureId,
                                  outcomeId,
                                  seqId)
    
    if(!any(is.na(llProfile))) {
      
      if(nrow(llProfile) == 0) {
        
        llProfile <- NA
        
      }
      
    }
    
    # Obtain corresponding null distribution
    
    nullDistFile <- paste0(exposureFolder, 
                           "/nullFits/tPeriod=",
                           seqId,
                           "/analysisId=",
                           analysisId,
                           "/nullFit_outcomeId=",
                           outcomeId,
                           "_exposureId=",
                           exposureId,
                           ".Rds")
    
    if(file.exists(nullDistFile)) {
      
      nullDist <- readRDS(nullDistFile)
      
      #Convert null to systematic error model
      
      if(!any(is.na(nullDist))) {
        
        nullModel <- EmpiricalCalibration::convertNullToErrorModel(nullDist)
        
      }
      
    } else {
      
      nullDist <- NA
      
    }
    
    ## Obtain calibrated statistics
    
    #if(!any(is.na(nullDist) | is.na(llProfile))) {
    if(!any(is.na(nullDist))) {
      
      # idx <- periodEstimates$analysisId == analysisId & 
      #   periodEstimates$outcomeId == outcomeId &
      #   periodEstimates$seqId == seqId &
      #   periodEstimates$exposureId == exposureId #index to write the results
      
      #if(length(which(idx == 1)) != 1) {stop("Something is wrong!")}
      
      ## Calibrate the statistics
      
      if(!is.na(periodEstimates$seLogRr[idx]) & abs(periodEstimates$logRr[idx]) <= 5) {
        
        # Obtain and save calibrated RR and 95% CI
        
        calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(periodEstimates$logRr[idx],
                                                                          periodEstimates$seLogRr[idx],
                                                                          nullModel)
        
        periodEstimates$calibratedLogRr[idx] <- calibratedCi$logRr
        periodEstimates$calibratedSeLogRr[idx] <- calibratedCi$seLogRr
        periodEstimates$calibratedRr[idx] <- exp(calibratedCi$logRr)
        periodEstimates$calibratedLb95[idx] <- exp(calibratedCi$logLb95Rr)
        periodEstimates$calibratedUb95[idx] <- exp(calibratedCi$logUb95Rr)
        
        # Obtain and save calibrated two-sided and one-sided p values
        
        calibratedP <- EmpiricalCalibration::calibrateP(nullDist,
                                                        periodEstimates$logRr[idx],
                                                        periodEstimates$seLogRr[idx],
                                                        twoSided = TRUE,
                                                        pValueOnly = TRUE)
        calibratedOneSidedP <- EmpiricalCalibration::calibrateP(nullDist,
                                                                periodEstimates$logRr[idx],
                                                                periodEstimates$seLogRr[idx],
                                                                twoSided = FALSE,
                                                                pValueOnly = TRUE)
        
        periodEstimates$calibratedP[idx] <- as.numeric(calibratedP)
        periodEstimates$calibratedOneSidedP[idx] <- as.numeric(calibratedOneSidedP)
        
        # Obtain and save calibrated LLR (most important)
        
        if(!any(is.na(llProfile))) {
        
        calibratedLlr <- EmpiricalCalibration::calibrateLlr(nullDist,
                                                            llProfile)
        periodEstimates$calibratedLlr[idx] <- as.numeric(calibratedLlr[1])
        
        } 
        
      }
      
    } 
    
    return(periodEstimates[idx,])
    
  }
  
}

calibratedLLRStore <- function(methodName,
                               analysisIds,
                               databaseId,
                               baseExposureIds,
                               outcomeIds,
                               clusters,
                               force=FALSE) {
  
  for(baseExposureId in baseExposureIds) {
    
    exposureFolder <- paste0("E:/Shounak_R/eumaeusTest_",
                             databaseId,
                             "_Shounak/",
                             methodName,
                             "/e_",
                             baseExposureId)
    
    newFileName <- paste0(exposureFolder, "/periodEstimatesWithCvsCalibrated.csv")
    
    if(!file.exists(newFileName) | force) {
      
      periodEstimatesFile <- paste0(exposureFolder, "/periodEstimatesWithCvs.csv")
      
      periodEstimates <- readr::read_csv(periodEstimatesFile)
      
      exposureIds <- unique(periodEstimates$exposureId)
      
      seqIds = c(1:max(periodEstimates$seqId))
      
      print(paste0("SeqId: ", seqIds))
      print(paste0("Method: ", methodName))
      
      # Create calibrated columns
      
      periodEstimates$calibratedRr <- NA
      periodEstimates$calibratedLb95 <- NA
      periodEstimates$calibratedUb95 <- NA
      periodEstimates$calibratedLogRr <- NA
      periodEstimates$calibratedSeLogRr <- NA
      periodEstimates$calibratedP <- NA
      periodEstimates$calibratedOneSidedP <- NA
      periodEstimates$calibratedLlr <- NA
      
      allEstimates <- list()
      #dummyIndex <- 0
      
      for(exposureId in exposureIds) {
        
        for(analysisId in analysisIds) {
          
          for(seqId in seqIds) {
            
            print(paste0("Carrying out LLR calibration for exposureId = ",
                         exposureId,
                         ", analysisId = ",
                         analysisId,
                         ", seqId = ",
                         seqId))
            
            #dummyIndex <- dummyIndex + 1
            
            settings = list("periodEstimates" = periodEstimates,
                            "exposureId" = exposureId,
                            "analysisId" = analysisId,
                            "seqId" = seqId,
                            "methodName" = methodName,
                            "databaseId" = databaseId,
                            "exposureFolder" = exposureFolder)
            
            clusterOutput <- ParallelLogger::clusterApply(clusters, 
                                                          outcomeIds,
                                                          clusterFunction,
                                                          settings)
            
            # R-bind individual node outputs
            
            clusterOutput <- dplyr::bind_rows(clusterOutput)
            
            # Save inside list
            
            #allEstimates[[dummyIndex]] <- clusterOutput
            allEstimates[[length(allEstimates) + 1]] <- clusterOutput
            
          }
          
        }
        
      }
      
      allEstimates <- dplyr::bind_rows(allEstimates)
      
      write.csv(allEstimates, paste0(exposureFolder, "/periodEstimatesWithCvsCalibrated.csv"))
      
    } else {
      
      print(paste0("File already exists!"))
      
    }
      
  }
  
}


extractLLProfile <- function(methodName,
                             analysisId,
                             databaseId,
                             exposureId,
                             outcomeId,
                             seqId) {
  
  baseExposureId <- as.numeric(substr(exposureId, 1, 5))
  exposureFolder = paste0("E:/Shounak_R/eumaeusTest_",
                          databaseId,
                          "_Shounak/",
                          methodName,
                          "/e_",
                          baseExposureId)
  
  if(!methodName %in% c("ConcurrentComparator", "ConcurrentComparator_1-28Days")) {
    
    if(methodName == "historicalComparator") {
      
      periodEstimates <- readr::read_csv(paste0(exposureFolder, "/periodEstimatesWithCvs.csv"))
      granularEstimates <- periodEstimates %>% filter(seqId == !!seqId,
                                                      outcomeId == !!outcomeId,
                                                      analysisId == !!analysisId)
      
      # Must obtain likelihood profiles on the fly, as they are not saved
      
      hcLLProfile <- Eumaeus:::computeHcProfile(granularEstimates)
      
      if(!any(is.na(hcLLProfile))) {
        
        point <- as.numeric(strsplit(hcLLProfile$point, ";")[[1]])
        value <- as.numeric(strsplit(hcLLProfile$value, ";")[[1]])
        
        llProfile <- data.frame("point" = point,
                                "value" = value)
        
      } else {
        
        llProfile <- NA
        
      }
      
    } else if(methodName == "sccs") {
      
      llProfileLoadDir <- paste0(exposureFolder, 
                                 "/",
                                 methodName,
                                 "Output_t",
                                 seqId,
                                 "/Analysis_",
                                 analysisId,
                                 "/SccsModel_e",
                                 exposureId,
                                 "_o",
                                 outcomeId,
                                 ".Rds")
      
      if(file.exists(llProfileLoadDir)) {
        
        llProfile <- readRDS(llProfileLoadDir)
        llProfile <- llProfile$logLikelihoodProfiles[[1]]
        
        if(!any(is.na(llProfile))) {
          
          point <- llProfile$point
          value <- llProfile$value
          
          llProfile <- data.frame("point" = point,
                                  "value" = value)
          
        } else {
          
          llProfile <- NA
          
        }
        
      } else {
        
        llProfile <- NA
        
      }
      
    } else if(methodName == "caseControl") {
      
      llProfileLoadDir <- paste0(exposureFolder, 
                                 "/ccOutput_t",
                                 seqId,
                                 "/Analysis_",
                                 analysisId,
                                 "/model_e",
                                 exposureId,
                                 "_o",
                                 outcomeId,
                                 ".Rds")
      
      if(file.exists(llProfileLoadDir)) {
        
        llProfile <- readRDS(llProfileLoadDir)
        llProfile <- llProfile$logLikelihoodProfile
        
        if(!any(is.na(llProfile))) {
          
          llProfile <- as.data.frame(llProfile)
          
          point <- as.numeric(rownames(llProfile))
          value <- llProfile$llProfile
          
          llProfile <- data.frame("point" = point,
                                  "value" = value)
          
        } else {
          
          llProfile <- NA
          
        }
        
      } else {
        
        llProfile <- NA
        
      }
      
    } else {
      
      stop("Have you run cohort method yet?")
      
    }
    
  } else {
    
    # Code to load concurrent comparator LL profiles
    
    llProfileLoadDir <- paste0(exposureFolder, 
                               "/ConcurrentComparatorOutput_t_",
                               seqId,
                               "/logLikelihoodFit_t",
                               exposureId,
                               "_c_",
                               outcomeId,
                               ".Rds")
    
    if(file.exists(llProfileLoadDir)) {
      
      llProfile <- readRDS(llProfileLoadDir)
      llProfile <- llProfile$logLikelihoodProfile
      
      if(!any(is.na(llProfile))) {
        
        point <- llProfile$point
        value <- llProfile$value
        
        llProfile <- data.frame("point" = point,
                                "value" = value)
        
      } else {
        
        llProfile <- NA
        
      }
      
    } else {
      
      llProfile <- NA
      
    }
    
  }
  
  return(llProfile)
  
}

