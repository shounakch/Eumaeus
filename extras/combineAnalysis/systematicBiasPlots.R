library(ggplot2)
library(dplyr)

#produce density plots comparing systematic errors across methods for a given exposure, data source.
#choose only final time point

## Read allEstimates file from locally saved version

allEstimates <- data.table::fread(input = "E:/Shounak_R/EumaeusAnalysis/allPastEstimates.csv",
                                  sep = ",")
# allEstimatesImputedPcs <- data.table::fread(input = "E:/Shounak_R/EumaeusAnalysis/allPastEstimatesImputedPcs.csv",
#                                             sep = ",")
# allEstimatesImputedPcsDescription <- data.table::fread(input = "E:/Shounak_R/EumaeusAnalysis/allPastImputedPcAnalysesDescription.csv",
#                                                        sep = ",")

############ ------ PLOT parameters -------#############

#maxTimePeriod=9 for Flu (21215), H1N1 (21184); 12 for Zoster (211983), HPV (211833); 7 for Covid (21216, 21217)

databaseId = "IBM_MDCD"
exposureIds = c(21215, 21184, 211983, 211833)
exposureNames = c("Seasonal flu (all)",
                  "H1N1pdm vaccination",
                  "Zoster (first or second)",
                  "HPV (first or second)")
maxTimePeriods = c(9, 9, 12, 12)
analysisIds = list("HistoricalComparatorAnalysisId" = 4,
                   "SCCSAnalysisId" = 2,
                   "CaseControlId" = 2,
                   "CohortMethodId" = 2)

exposureIndex = 3 #just change this

exposureId = exposureIds[exposureIndex]
maxTimePeriod = maxTimePeriods[exposureIndex] #depends on exposureId
exposureName = exposureNames[exposureIndex] #depends on exposureId

pl <- plotSystematicErrorDistributionsOldExposures(databaseId,
                                                   exposureId,
                                                   maxTimePeriod,
                                                   analysisIds,
                                                   exposureName,
                                                   mcmc = TRUE)

############### ------------ PLOT CODES #####################

plotSystematicErrorDistributionsOldExposures <- function(databaseId, 
                                                         exposureId,
                                                         maxTimePeriod,
                                                         analysisIds,
                                                         exposureName,
                                                         mcmc = FALSE) {
  
  matchDataSourceName = rbind(c("IBM_MDCR", "truven_mdcr"),
                              c("IBM_MDCD", "truven_mdcd"),
                              c("CCAE", "truven_ccae"),
                              c("OptumEhr", "optum_ehr"),
                              c("OptumDod", "optum_extended_dod"))
  
  negativeControlIds = read.csv("E:/Shounak_R/Eumaeus/inst/settings/NegativeControls.csv")
  outcomeIds = negativeControlIds$outcomeId
  
  analysisIds = c(1, #dummy analysisId for concurrent comparator
                  analysisIds$HistoricalComparatorAnalysisId,
                  analysisIds$CohortMethodId,
                  analysisIds$CaseControlId,
                  analysisIds$SCCSAnalysisId)
  
  baseExposureId = as.numeric(substr(exposureId, 1, 5))
  
  xVals = seq(log(0.1), log(4), length.out = 100)
  
  file.name = paste0("E:/Shounak_R/EumaeusAnalysis/sysBias_", 
                     databaseId, 
                     "_", 
                     exposureId,
                     ".RData")
  
  if(!file.exists(file.name)) {
    
    allDensValues = list()
    method = list()
    allSysErrorMeans = rep(0, 5)
    allSysErrorSds = rep(0, 5)
    
    # first produce concurrent comparator values
    
    ccDatabaseId = matchDataSourceName[which(matchDataSourceName[,1] == databaseId), 2]
    file.name.CC = paste0("E:/Shounak_R/eumaeusTest_", ccDatabaseId, "_Shounak/ConcurrentComparator_1-28Days/e_", baseExposureId, "/estimates_t", maxTimePeriod, ".csv")
    ConcurrentComparatorResult = readr::read_csv(file = file.name.CC)
    
    ConcurrentComparatorResult = ConcurrentComparatorResult %>% filter(exposureId == !!exposureId)
    
    na.ind = which(abs(ConcurrentComparatorResult$logRr) > 5 | 
                     is.na(ConcurrentComparatorResult$seLogRr))
    
    #allLogRrValues = c(allLogRrValues, as.numeric(ConcurrentComparatorResult$logRr))
    
    if(mcmc == TRUE) {
      
      sysErrorDist = EmpiricalCalibration::fitMcmcNull(ConcurrentComparatorResult$logRr[-na.ind],
                                                       ConcurrentComparatorResult$seLogRr[-na.ind],
                                                       iter = 10^5)
      
      allSysErrorMeans[1] = sysErrorDist[1]
      allSysErrorSds[1] = 1 / sqrt(sysErrorDist[2])
      
    } else {
      
      sysErrorDist = EmpiricalCalibration::fitNull(ConcurrentComparatorResult$logRr[-na.ind], ConcurrentComparatorResult$seLogRr[-na.ind])
      
      allSysErrorMeans[1] = sysErrorDist[1]
      allSysErrorSds[1] = sysErrorDist[2]
      
    }
    
    allDensValues[[1]] = dnorm(x = xVals, mean = allSysErrorMeans[1], sd = allSysErrorSds[1])
    
    method[[1]] = rep("ConcurrentComparator", length(xVals))
    
    # Now produce other method values
    
    methodNames = c("HistoricalComparator", "SCCS", "CaseControl", "CohortMethod")
    
    for(i in 1:length(methodNames)) {
      
      methodName = methodNames[i]
      
      analysisId = analysisIds[i+1]
      
      methodEstimates <- allEstimates %>% dplyr::filter(databaseId == !!databaseId,
                                                        exposureId == !!exposureId,
                                                        method == !!methodName,
                                                        analysisId == !!analysisId,
                                                        outcomeId %in% outcomeIds,
                                                        periodId == !!maxTimePeriod)
      
      if(mcmc == TRUE) {
        
        sysErrorDist = EmpiricalCalibration::fitMcmcNull(methodEstimates$logRr,
                                                         methodEstimates$seLogRr,
                                                         iter = 10^5)
        
        allSysErrorMeans[i+1] = sysErrorDist[1]
        allSysErrorSds[i+1] = 1 / sqrt(sysErrorDist[2])
        
      } else {
        
        sysErrorDist = EmpiricalCalibration::fitNull(methodEstimates$logRr,
                                                     methodEstimates$seLogRr)
        
        allSysErrorMeans[i+1] = sysErrorDist[1]
        allSysErrorSds[i+1] = sysErrorDist[2]
        
      }
      
      allDensValues[[i+1]] = dnorm(x = xVals, mean = allSysErrorMeans[i+1], sd = allSysErrorSds[i+1])
      
      method[[i+1]] = rep(methodName, length(xVals))
      
    }
    
    allDensValues = unlist(allDensValues)
    method = unlist(method)
    
    allDensValuesDf = data.frame("xVals" = rep(xVals, 5),
                                 "density" = allDensValues,
                                 "Method" = method)
    
  } else {
    
    #allDensValuesDf = readRDS(file = file.name)$allDensValuesDf
    allDensValuesDf = readRDS(file = file.name)$data
    
  }
  
  breaks = c(0.25, 0.5, 1, 2, 4)
  
  # densPlot <- ggplot(allDensValuesDf, aes(x = xVals, group = Method)) +
  #   geom_line(aes(x = xVals, y = density, color = Method), linewidth = 2, alpha = 1) +
  #   geom_ribbon(aes(ymin = 0, ymax = density, fill = Method), alpha = 0.85) +
  #   geom_vline(aes(xintercept=0), linetype = "dashed", linewidth = 2, color = "Black") +
  #   geom_hline(aes(yintercept=0), linewidth = 2) +
  #   scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")) +
  #   scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1")) +
  #   #scale_alpha_manual(values = c(0, 0, 0, 0, 1)) +
  #   ylab("Density") +
  #   #xlab("Risk ratio (on log-scale)") +
  #   ggtitle(paste0(exposureName, ", ", databaseId)) +
  #   scale_x_continuous("Risk-ratio (on log-scale)", 
  #                      breaks = log(breaks), labels = breaks, limits = c(log(0.25), log(4))) +
  #   theme_minimal() +
  #   theme(text = element_text(size=36))
  
  #earlier plot
  # densPlot <- ggplot(allDensValuesDf, aes(x = xVals, group = Method)) +
  #   geom_line(data = subset(allDensValuesDf, Method != "SCCS"), aes(x = xVals, y = density, color = Method), linewidth = 2, alpha = 1) +
  #   geom_ribbon(data = subset(allDensValuesDf, Method != "SCCS"), aes(ymin = 0, ymax = density, fill = Method), alpha = 0.8) +
  #   # scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")[1:4]) +
  #   # scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1")[1:4]) +
  #   geom_ribbon(data = subset(allDensValuesDf, Method == "SCCS"), aes(ymin = 0, ymax = density, fill = Method), fill = "White", alpha = 1) +
  #   geom_line(data = subset(allDensValuesDf, Method == "SCCS"), aes(x = xVals, y = density, color = Method), linewidth = 2, alpha = 1) + #, color = wesanderson::wes_palette("Darjeeling1")[5]) +
  #   geom_ribbon(data = subset(allDensValuesDf, Method == "SCCS"), aes(ymin = 0, ymax = density, fill = Method), alpha = 0.8) + #, fill = wesanderson::wes_palette("Darjeeling1")[5], alpha = 0.8) + 
  #   scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")) +
  #   scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1")) +
  #   geom_vline(aes(xintercept=0), linetype = "dashed", linewidth = 2, color = "Black") +
  #   geom_hline(aes(yintercept=0), linewidth = 2) +
  #   ylab("Density") +
  #   ggtitle(paste0(exposureName, ", ", databaseId)) +
  #   scale_x_continuous("Risk-ratio (on log-scale)", 
  #                      breaks = log(breaks), 
  #                      labels = breaks, 
  #                      limits = c(log(0.25), log(4))) +
  #   theme_minimal() +
  #   theme(text = element_text(size=36)) 
  
  #latest plot
  densPlot <- ggplot(allDensValuesDf, aes(x = xVals, group = Method)) +
    geom_line(data = subset(allDensValuesDf, Method != "ConcurrentComparator"), aes(x = xVals, y = density, color = Method), linewidth = 2, alpha = 1) +
    geom_ribbon(data = subset(allDensValuesDf, Method != "ConcurrentComparator"), aes(ymin = 0, ymax = density, fill = Method), alpha = 0.8) +
    # scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")[1:4]) +
    # scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1")[1:4]) +
    geom_ribbon(data = subset(allDensValuesDf, Method == "ConcurrentComparator"), aes(ymin = 0, ymax = density, fill = Method), fill = "White", alpha = 1) +
    geom_line(data = subset(allDensValuesDf, Method == "ConcurrentComparator"), aes(x = xVals, y = density, color = Method), linewidth = 2, alpha = 1) + #, color = wesanderson::wes_palette("Darjeeling1")[5]) +
    geom_ribbon(data = subset(allDensValuesDf, Method == "ConcurrentComparator"), aes(ymin = 0, ymax = density, fill = Method), alpha = 0.8) + #, fill = wesanderson::wes_palette("Darjeeling1")[5], alpha = 0.8) + 
    scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")) +
    scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1")) +
    geom_vline(aes(xintercept=0), linetype = "dashed", linewidth = 3, color = "Black") +
    geom_hline(aes(yintercept=0), linewidth = 2) +
    ylab("Density") +
    ggtitle(paste0(exposureName, ", ", databaseId)) +
    scale_x_continuous("Risk-ratio (on log-scale)", 
                       breaks = log(breaks), 
                       labels = breaks, 
                       limits = c(log(0.25), log(4))) +
    theme_minimal() +
    theme(text = element_text(size=50))
          # legend.position = "bottom",  # Position the legend at the bottom
          # legend.direction = "horizontal",  # Set the legend direction to horizontal
          # legend.key.size = unit(1, "cm"),  # Increase the size of the legend keys
          # legend.text = element_text(size = 20),
          # legend.spacing.x = unit(0.5, 'cm'))  # Optionally adjust the size of the legend text
          
  #saveRDS(densPlot, file = file.name)
  
  print(densPlot)
  
  densPlot2 <- densPlot + guides(
    color = guide_legend(order = 5),
    fill = guide_legend(order = 5)
  )
  
  print(densPlot2)
  
  output = list("densPlot" = densPlot,
                "allDensValuesDf" = allDensValuesDf)
  
  return(output)
  
}
