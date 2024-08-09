source("E:/Shounak_R/Eumaeus/extras/combineAnalysis/calibratedCvsCovid.R")

#returnNULLScript <- function(nullArg = NULL) {

library(DatabaseConnector)
library(dplyr)
library(ggplot2)

#maxTimePeriod=9 for Flu (21215), H1N1 (21184); 12 for Zoster (211983), HPV (211833); 7 for Covid (21216, 21217)

#exposure 21216 - Name: COVID-19 vaccination (BNT126b2)
#exposure 21217 - Name: COVID-19 vaccination (mRNA-1273)

databaseId = "IBM_MDCD"
exposureIds = c(21216, 21217)
maxTimePeriod = 7 #depends on exposureId
analysisIds = list("HistoricalComparatorAnalysisId" = 4,
                   "SCCSAnalysisId" = 2,
                   "CaseControlId" = 2)

exposureNames = c("COVID-19 vaccination (BNT126b2)",
                  "COVID-19 vaccination (mRNA-1273)")

exposureIndex = 1
trueEffectSize = 4

exposureId = exposureIds[exposureIndex]
exposureName = exposureNames[exposureIndex]

plotObject <- plotType1ErrorAndPowerAcrossTime(databaseId,
                                               exposureId,
                                               trueEffectSize,
                                               analysisIds,
                                               maxTimePeriod,
                                               exposureName)

#### figure out why SCCS/Case Control type 1 errors are so high even after calibration!!!!!!

#}