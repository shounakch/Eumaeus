#source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/calibrateLLR/obtainType1Error.R")
#source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/calibrateLLR/obtainPower.R")

sourceFolder <- "~/Documents/Eumaeus-project/Eumaeus/extras/calibrationCodes/calibrateLLR/"

source(paste0(sourceFolder, "/obtainType1Error.R"))
source(paste0(sourceFolder, "/obtainPower.R"))

plotType1ErrorPower <- function(databaseId,
                                exposureId,
                                allResults,
                                analysisIds,
                                exposureName,
                                yVanish,
                                yTextVanish,
                                powerIntercept = 0.50,
                                powerBreaks = seq(0,1,by=0.25),
                                colorPalette = wesanderson::wes_palette("Darjeeling1")[-4],
                                fontSize = 25) {
  
  outputDf <- list()
  methodNames <- c("ConcurrentComparator_1-28Days", "SCCS", "HistoricalComparator", "CaseControl")
  
  for(method in methodNames) {
    
    outputDfUnit <- allResults %>% filter(databaseId == !!databaseId,
                                          exposureId == !!exposureId,
                                          method == !!method,
                                          analysisId == !!analysisIds[[method]])
    
    outputDf[[length(outputDf) + 1]] <- outputDfUnit
    
  }
  
  outputDf <- bind_rows(outputDf)
  outputDf$method[outputDf$method == "ConcurrentComparator_1-28Days"] = "ConcurrentComparator"
  maxTimePeriod <- max(outputDf$seqId)
  outputDf$Method <- outputDf$method
  
  type1ErrorPlot <- ggplot(outputDf, aes(x = seqId, 
                                         y = calibratedType1Error, 
                                         color = Method)) +
    geom_line(size = 3) + 
    geom_point(size = 2) +  # Points with colors based on Method (preserve legend)
    geom_point(size = 3, color = "black", show.legend = FALSE) +  # Black points on top, no legend
    geom_hline(aes(yintercept = 0.05), size = 2, linetype = "dashed") +
    scale_y_continuous("Type 1 Error, Calibrated", breaks = c(0, 0.05, seq(0.1, 0.2, by=0.05)), limits = c(0, 0.2)) +
    scale_x_continuous("Months", breaks = c(1:maxTimePeriod)) +
    scale_color_manual(values = colorPalette) +
    ggtitle(exposureName) +
    theme_minimal() +
    guides(color = guide_legend(override.aes = list(
      linetype = 0,       # Remove lines from legend
      shape = 22,         # Filled box for legend
      fill = colorPalette,  # Specify fill colors
      size = 8            # Adjust size of boxes
    ))) +
    theme(text = element_text(size=fontSize),
          axis.text.y = element_text(size=fontSize, margin = margin(r = 10)),
          plot.title = element_text(hjust=0.1),
          axis.text.x = element_text(size=fontSize),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank()) 
  
  #print(type1ErrorPlot)
  
  type1ErrorPlotUncalibrated <- ggplot(outputDf, aes(x = seqId, 
                                                     y = uncalibratedType1Error, 
                                                     color = Method)) +
    geom_line(size = 3) + 
    geom_point(size = 2) +  # Points with colors based on Method (preserve legend)
    geom_point(size = 3, color = "black", show.legend = FALSE) +  # Black points on top, no legend
    geom_hline(aes(yintercept = 0.05), size = 2, linetype = "dashed") +
    scale_y_continuous("Type 1 Error, Uncalibrated", breaks = c(0.05, 0.5, 1), limits = c(0, 1)) +
    scale_x_continuous("Months", breaks = 1:maxTimePeriod) +
    scale_color_manual(values = colorPalette) +
    ggtitle(exposureName) +
    theme_minimal() +
    guides(color = guide_legend(override.aes = list(
      linetype = 0,       # Remove lines from legend
      shape = 22,         # Filled box for legend
      fill = colorPalette,  # Specify fill colors
      size = 8            # Adjust size of boxes
    ))) +
    theme(text = element_text(size=fontSize),
          axis.text.y = element_text(size=fontSize, margin = margin(r = 10)),
          plot.title = element_text(hjust=0.1),
          axis.text.x = element_text(size=fontSize),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank()) 
  
  #print(type1ErrorPlotUncalibrated)
  
  power2 <- ggplot(outputDf, aes(x = seqId, 
                                 y = PowerTrueEffectSize2, 
                                 color = Method)) +
    geom_line(size = 3) + 
    geom_point(size = 2) +  # Points with colors based on Method (preserve legend)
    geom_point(size = 3, color = "black", show.legend = FALSE) +  # Black points on top, no legend
    geom_hline(aes(yintercept = powerIntercept), size = 2, linetype = "dashed") +
    scale_y_continuous("Power, True Effect Size = 2", breaks = powerBreaks, limits = c(0, 1)) +
    scale_color_manual(values = colorPalette) +
    ggtitle(exposureName) +
    scale_x_continuous(breaks = 1:maxTimePeriod) +
    theme_minimal() +
    guides(color = guide_legend(override.aes = list(
      linetype = 0,       # Remove lines from legend
      shape = 22,         # Filled box for legend
      fill = colorPalette,  # Specify fill colors
      size = 8            # Adjust size of boxes
    ))) +
    theme(text = element_text(size=fontSize),
          axis.text.y = element_text(size=fontSize, margin = margin(r = 10)),
          plot.title = element_text(hjust=0.1),
          axis.text.x = element_text(size=fontSize),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank()) 
  
  #print(power2)
  
  power4 <- ggplot(outputDf, aes(x = seqId, 
                                 y = PowerTrueEffectSize4, 
                                 color = Method)) +
    geom_line(size = 3) + 
    geom_point(size = 2) +  # Points with colors based on Method (preserve legend)
    geom_point(size = 3, color = "black", show.legend = FALSE) +  # Black points on top, no legend
    geom_hline(aes(yintercept = powerIntercept), size = 2, linetype = "dashed") +
    scale_y_continuous("Power, True Effect Size = 4", breaks = powerBreaks, limits = c(0, 1)) +
    scale_color_manual(values = colorPalette) +
    ggtitle(exposureName) +
    scale_x_continuous(breaks = 1:maxTimePeriod) +
    theme_minimal() +
    guides(color = guide_legend(override.aes = list(
      linetype = 0,       # Remove lines from legend
      shape = 22,         # Filled box for legend
      fill = colorPalette,  # Specify fill colors
      size = 8            # Adjust size of boxes
    ))) +
    theme(text = element_text(size=fontSize),
          axis.text.y = element_text(size=fontSize, margin = margin(r = 10)),
          plot.title = element_text(hjust=0.1),
          axis.text.x = element_text(size=fontSize),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank()) 
  
  #print(power4)
  
  if(yVanish) {
    
    type1ErrorPlot <- type1ErrorPlot + theme(axis.title.y = element_blank())
    type1ErrorPlotUncalibrated <- type1ErrorPlotUncalibrated + theme(axis.title.y = element_blank())
    power2 <- power2 + theme(axis.title.y = element_blank())
    power4 <- power4 + theme(axis.title.y = element_blank())
    
  }
  
  if(yTextVanish) {
    
    type1ErrorPlot <- type1ErrorPlot + theme(axis.text.y = element_blank())
    type1ErrorPlotUncalibrated <- type1ErrorPlotUncalibrated + theme(axis.text.y = element_blank())
    power2 <- power2 + theme(axis.text.y = element_blank())
    power4 <- power4 + theme(axis.text.y =  element_blank())
    
  }
  
  allPlots <- list("Type1ErrorCalibrated" = type1ErrorPlot,
                   "Type1ErrorUncalibrated" = type1ErrorPlotUncalibrated,
                   "Power2" = power2,
                   "Power4" = power4)
  
  return(allPlots)
  
}

saveType1ErrorPowerDf <- function(databaseIds,
                                  allPastEstimates,
                                  trueEffectSizes) {
  
  oldExposureIds <- c(211831, 211832, 211833, 21184, 21185, 211981, 211982, 211983, 21214, 21215)
  newExposureIds <- c(21216, 21217)
  allExposureIds <- c(oldExposureIds, newExposureIds)
  
  oldMethods <- c("SCCS", "HistoricalComparator", "CaseControl")
  newMethods <- c("ConcurrentComparator_1-28Days")
  
  allMethods <- c(oldMethods, newMethods)
  
  databaseIdDirectory <- rbind(c("OptumEhr", "optum_ehr"),
                               c("IBM_MDCD", "truven_mdcd"),
                               c("IBM_MDCR", "truven_mdcr"),
                               c("OptumDod", "optum_extended_dod"),
                               c("CCAE", "truven_ccae"))
  
  methodIdDirectory <- rbind(c("ConcurrentComparator_1-28Days", "ConcurrentComparator_1-28Days"),
                             c("SCCS", "sccs"),
                             c("HistoricalComparator", "historicalComparator"),
                             c("CaseControl", "caseControl"))
  
  outcomeIds <- readr::read_csv(system.file("settings/NegativeControls.csv", package = "Eumaeus"))
  outcomeIds <- outcomeIds$outcomeId
  
  allResults <- list()
  
  #databaseId <- databaseIds[1]
  for(databaseId in databaseIds) {
    
    #exposureId <- allExposureIds[1]
    for(exposureId in allExposureIds) {
      
      #method <- allMethods[1]
      for(method in allMethods) {
        
        print(paste0("Saving type 1 error and power for database ",
                     databaseId,
                     ", exposure ",
                     exposureId,
                     ", method ",
                     method,
                     "."))
        
        if(method %in% oldMethods & exposureId %in% oldExposureIds) {
          
          ##use past estimates
          
          periodEstimates <- allPastEstimates %>% filter(databaseId == !!databaseId,
                                                         exposureId == !!exposureId,
                                                         method == !!method,
                                                         outcomeId %in% outcomeIds)
          
          maxTimePeriod <- max(periodEstimates$periodId)
          
          analysisIds <- sort(unique(periodEstimates$analysisId))
          
          #analysisId <- analysisIds[1]
          for(analysisId in analysisIds) {
            
            type1Errors <- obtainType1Error(periodEstimates, 
                                            analysisId,
                                            exposureId)
            
            powers <- obtainPower(periodEstimates,
                                  analysisId,
                                  exposureId,
                                  trueEffectSizes)
            
            unitDf <- data.frame("databaseId" = rep(databaseId, maxTimePeriod))
            unitDf$exposureId <- exposureId
            unitDf$method <- method
            unitDf$analysisId <- analysisId
            
            unitDf$seqId <- 1:maxTimePeriod
            unitDf$uncalibratedType1Error <- as.numeric(type1Errors[type1Errors$Type == "Uncalibrated", paste0("time", 1:maxTimePeriod)])
            unitDf$calibratedType1Error <- as.numeric(type1Errors[type1Errors$Type == "Calibrated", paste0("time", 1:maxTimePeriod)])
            
            for(trueEffectSize in trueEffectSizes) {
              
              unitDf[,paste0("PowerTrueEffectSize",trueEffectSize)] <- as.numeric(powers[powers$trueEffectSize == trueEffectSize, paste0("time", 1:maxTimePeriod)])
              
            }
            
            allResults[[length(allResults) + 1]] <- unitDf
            
          }
          
        } else {
          
          ## use local estimates
          
          ## first load periodEstimates file
          
          convertedDatabaseId <- databaseIdDirectory[which(databaseIdDirectory[,1] == databaseId),2]
          convertedMethod <- methodIdDirectory[which(methodIdDirectory[,1] == method),2]
          baseExposureId <- as.numeric(substr(exposureId, 1, 5)) #should be same as exposureId for covid exposure IDs
          
          periodEstimatesFile <- paste0("E:/Shounak_R/eumaeusTest_",
                                        convertedDatabaseId,
                                        "_Shounak/",
                                        convertedMethod,
                                        "/e_",
                                        baseExposureId,
                                        "/periodEstimatesWithCvsCalibrated.csv")
          
          if(!file.exists(periodEstimatesFile)) {
            
            stop("No period estimates file found for database ", 
                 convertedDatabaseId,
                 ", method ",
                 convertedMethod,
                 ", exposure ", 
                 baseExposureId)
            
          } else {
            
            periodEstimates <- readr::read_csv(periodEstimatesFile)
            
            maxTimePeriod <- max(periodEstimates$seqId)
            
            analysisIds <- sort(unique(periodEstimates$analysisId))
            
            analysisId <- analysisIds[1]
            for(analysisId in analysisIds) {
              
              type1Errors <- obtainType1Error(periodEstimates, 
                                              analysisId,
                                              exposureId)
              
              powers <- obtainPower(periodEstimates,
                                    analysisId,
                                    exposureId,
                                    trueEffectSizes)
              
              unitDf <- data.frame("databaseId" = rep(databaseId, maxTimePeriod))
              unitDf$exposureId <- exposureId
              unitDf$method <- method
              unitDf$analysisId <- analysisId
              
              unitDf$seqId <- 1:maxTimePeriod
              unitDf$uncalibratedType1Error <- as.numeric(type1Errors[type1Errors$Type == "Uncalibrated", paste0("time", 1:maxTimePeriod)])
              unitDf$calibratedType1Error <- as.numeric(type1Errors[type1Errors$Type == "Calibrated", paste0("time", 1:maxTimePeriod)])
              
              for(trueEffectSize in trueEffectSizes) {
                
                unitDf[,paste0("PowerTrueEffectSize",trueEffectSize)] <- as.numeric(powers[powers$trueEffectSize == trueEffectSize, paste0("time", 1:maxTimePeriod)])
                
              }
              
              allResults[[length(allResults) + 1]] <- unitDf
              
            }
            
          }
          
        }
        
      }
      
    }
    
  }
  
  allResults = bind_rows(allResults)
  write.csv(allResults, "E:/Shounak_R/EumaeusAnalysis/allType1ErrorAndPowerResults.csv")
  
}
