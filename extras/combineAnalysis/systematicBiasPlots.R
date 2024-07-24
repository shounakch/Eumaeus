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

databaseId = "OptumEhr"
exposureId = 21184
maxTimePeriod = 9 #depends on exposureId
analysisIds = list("HistoricalComparatorAnalysisId" = 4,
                   "SCCSAnalysisId" = 2,
                   "CaseControlId" = 2,
                   "CohortMethodId" = 2)

exposureName = "H1N1pdm vaccination"

plotSystematicErrorDistributionsOldExposures(databaseId,
                                             exposureId,
                                             maxTimePeriod,
                                             analysisIds,
                                             exposureName)

############### ------------ PLOT CODES #####################

plotSystematicErrorDistributionsOldExposures <- function(databaseId, 
                                                         exposureId,
                                                         maxTimePeriod,
                                                         analysisIds,
                                                         exposureName) {
  
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
  
  allLogRrValues = rep(0,93)
  
  # first produce concurrent comparator values
  
  ccDatabaseId = matchDataSourceName[which(matchDataSourceName[,1] == databaseId), 2]
  file.name = file.name = paste0("E:/eumaeusTest_", ccDatabaseId, "_Shounak/ConcurrentComparator/e_", baseExposureId, "/estimates_t", maxTimePeriod, ".csv")
  ConcurrentComparatorResult = readr::read_csv(file = file.name)
  
  allLogRrValues = as.numeric(ConcurrentComparatorResult$logRr)
  method = rep("ConcurrentComparator", length(allLogRrValues))
  
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
    
    allLogRrValues = c(allLogRrValues, methodEstimates$logRr)
    method = c(method, rep(methodName, length(methodEstimates$logRr)))
    
  }
  
  allDensValuesDf = data.frame("logRr" = allLogRrValues,
                               "Method" = method)
  
  allDensValuesDf = allDensValuesDf %>% filter(abs(logRr) <= 5)
  
  densPlot <- ggplot(allDensValuesDf, aes(x = exp(logRr), group = Method)) +
    #geom_density(aes(color=Method), linewidth = 1.5, show_guide=FALSE) +
    stat_density(aes(colour=Method), geom="line", position="identity", linewidth = 1.5) +
    geom_vline(aes(xintercept=1), linetype = "dashed") + theme_minimal() +
    scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")) +
    ylab("Density") +
    xlab("Risk Ratio") +
    ggtitle(paste0(exposureName, ", ", databaseId)) +
    xlim(c(0.0001, 5)) +
    theme(text = element_text(size=22))
  
  print(densPlot)
  
  return(densPlot)
  
}
