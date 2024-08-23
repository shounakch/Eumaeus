# Obtain type 1 error plots for all exposures

library(dplyr)
library(ggplot2)
library(patchwork)

source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/calibrateLLR/type1ErrorPowerPlots.R")

databaseId <- "OptumEhr"
exposuresToPlot <- c(21216, 21217, 21215, 211983, 21184, 211833)
exposuresToPlotNames <- c("COVID-19 (BNT126b2)", 
                          "COVID-19 (mRNA-1273)",
                          "Seasonal flu (all)",
                          "Zoster (first or second dose)",
                          "H1N1pdm",
                          "HPV (first or second dose)")

allResults <- readr::read_csv("E:/Shounak_R/EumaeusAnalysis/allType1ErrorAndPowerResults.csv")
analysisIds <- list("ConcurrentComparator_1-28Days" = 3,
                    "SCCS" = 2,
                    "HistoricalComparator" = 4,
                    "CaseControl" = 2)

allPlots <- list()

for(exposureId in exposuresToPlot) {
  
  exposureName <- exposuresToPlotNames[which(exposuresToPlot == exposureId)]
  
  if(exposureId %in% c(21217, 211983, 211833)) { #2nd column plots
    
    yTextVanish <- TRUE
    
  } else {
    
    yTextVanish <- FALSE
    
  }
  
  #yVanish <- TRUE
  
  allPlots[[exposureName]] <- plotType1ErrorPower(databaseId,
                                                  exposureId,
                                                  allResults,
                                                  analysisIds,
                                                  exposureName,
                                                  yVanish=TRUE,
                                                  yTextVanish)
  
}

## Sum up all Type 1 Error (calibrated) and Power plots together

plType1ErrorCalibrated <- list()
plPower <- list()

for(exposureName in exposuresToPlotNames) {
  
  plType1ErrorCalibrated[[length(plType1ErrorCalibrated) + 1]] <- allPlots[[exposureName]]$Type1ErrorCalibrated
  plPower[[length(plPower) + 1]] <- allPlots[[exposureName]]$Power2
  
}

#### Arrange in grid

## Type 1 Error

plType1ErrorCalibratedCombined <- wrap_plots(plType1ErrorCalibrated, nrow = 3, ncol = 2) &
  theme(legend.position = "bottom",
        legend.text = element_text(size=30),
        legend.title = element_blank(),
        axis.title.x = element_text(size=30),
        axis.title.y = element_text(size=30, angle=90, margin=margin(r=10))) 

plType1ErrorCalibratedCombined <- plType1ErrorCalibratedCombined +
  plot_annotation(title = "Type 1 Errors across time, after calibration",
                  theme = theme(plot.title = element_text(size = 40, hjust = 0.5, margin=margin(b=20)))) &
  labs(x = "Months", y = "Type 1 Error, Calibrated") 

plType1ErrorCalibratedCombined <- plType1ErrorCalibratedCombined +
  plot_layout(guides = "collect", 
              axis_titles = "collect",
              axes="collect_y")

print(plType1ErrorCalibratedCombined)

ggsave(plType1ErrorCalibratedCombined,
       filename = "E:/Shounak_R/Eumaeus/extras/finalPlots/type1ErrorCalibrated.eps",
       width=450,
       height=550,
       units="mm",
       dpi = 400)

## Power

plPowerCombined <- wrap_plots(plPower, nrow = 3, ncol = 2) &
  theme(legend.position = "bottom",
        legend.text = element_text(size=30),
        legend.title = element_blank(),
        axis.title.x = element_text(size=30),
        axis.title.y = element_text(size=30, angle=90, margin=margin(r=10))) 

plPowerCombined <- plPowerCombined +
  plot_annotation(title = "Power of detection across time, after calibration",
                  theme = theme(plot.title = element_text(size = 40, hjust = 0.5, margin=margin(b=20)))) &
  labs(x = "Months", y = "Power of detection") 

plPowerCombined <- plPowerCombined +
  plot_layout(guides = "collect", 
              axis_titles = "collect",
              axes="collect_y")

print(plPowerCombined)

ggsave(plPowerCombined,
       filename = "E:/Shounak_R/Eumaeus/extras/finalPlots/power2.eps",
       width=450,
       height=550,
       units="mm",
       dpi = 400)

#### Systematic error

## Across Optum EHR, MDCD * 2 covid vaccines

lengthGrid <- 100
sysErrorOutput <- list()
xseq <- seq(log(0.25), log(4), length.out = lengthGrid)
nullFitAnalysisIds <- c(3, 2, 4, 2)
methodNames <- c("ConcurrentComparator_1-28Days", "sccs", "historicalComparator", "caseControl")

for(databaseId in c("optum_ehr", "truven_mdcd")) {
  
  for(exposureId in c(21216, 21217)) {
    
    baseExposureId <- as.numeric(substr(exposureId, 1, 5))
    
    maxTimePeriod <- 7 #for covid vaccine
    
    for(method in methodNames) {
      
      periodEstimatesFileName <- paste0("E:/Shounak_R/eumaeusTest_",
                                        databaseId,
                                        "_Shounak/",
                                        method,
                                        "/e_",
                                        baseExposureId,
                                        "/periodEstimatesWithCvsCalibrated.csv")
      
      periodEstimates <- readr::read_csv(periodEstimatesFileName)
      
      subset <- periodEstimates %>% filter(analysisId == !!nullFitAnalysisIds[which(methodNames == method)],
                                           seqId == !!maxTimePeriod,
                                           exposureId == !!exposureId)
      
      legitIndices <- !is.na(subset$seLogRr) & abs(subset$logRr) <= 5
      print(paste0("Length of indices to construct null distribution is ", length(legitIndices)))
      
      nullDist <- EmpiricalCalibration::fitNull(subset$logRr[legitIndices],
                                                subset$seLogRr[legitIndices])
      
      if(nullDist[2] <= 0.01) {
        
        nullDist <- EmpiricalCalibration::fitMcmcNull(subset$logRr[legitIndices],
                                                      subset$seLogRr[legitIndices])
        
      }
      
      # Start storing things
      
      unitDf <- data.frame("Method" = rep(method, lengthGrid))
      unitDf$Point <- xseq
      unitDf$exposureId <- exposureId
      unitDf$databaseId <- databaseId
      
      if(is(nullDist, "null")) {
        
        unitDf$Density <- dnorm(xseq, mean = nullDist[1], sd = nullDist[2])
        
      } else {
        
        unitDf$Density <- dnorm(xseq, mean = nullDist[1], sd = 1 / sqrt(nullDist[2]))
        
      }
      
      sysErrorOutput[[length(sysErrorOutput) + 1]] <- unitDf
      
    }
    
  }
  
}

sysErrorOutput <- bind_rows(sysErrorOutput)

write.csv(bind_rows(sysErrorOutput), paste0("E:/Shounak_R/EumaeusAnalysis/systematicErrorDensityData.csv"))

## Now plot systematic error distributions

sysErrorPlots <- list()
colorPalette <- wesanderson::wes_palette("Darjeeling1")[-4]
exposureNames <- c("COVID-19 (BNT126b2)", "COVID-19 (mRNA-1273)")
breaks <- c(0.25, 0.5, 1, 2, 4)
densMax <- 3 #hides the rest of y axis outside (0, densMax)

for(databaseId in c("optum_ehr", "truven_mdcd")) {
  
  newDatabaseId <- ifelse(databaseId == "optum_ehr", "Optum EHR", "MDCD")
  
  for(exposureId in c(21216, 21217)) {
    
    exposureName <- ifelse(exposureId == 21216, "COVID-19 (BNT126b2)", "COVID-19 (mRNA-1273)")
    
    subsetDf <- sysErrorOutput %>% filter(databaseId == !!databaseId,
                                          exposureId == !!exposureId)
    
    subsetDf$Method[subsetDf$Method == "ConcurrentComparator_1-28Days"] = "ConcurrentComparator"
    
    #subsetDf <- subsetDf %>% filter(Density <= 5)
    
    densPlot <- ggplot(subsetDf, aes(x = Point, group = Method)) +
      geom_line(data = subset(subsetDf, Method != "ConcurrentComparator"), aes(x = Point, y = Density, color = Method), size = 2, alpha = 1, show.legend=FALSE) +
      geom_ribbon(data = subset(subsetDf, Method != "ConcurrentComparator"), aes(ymin = 0, ymax = Density, fill = Method), alpha = 0.8) +
      # scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")[1:4]) +
      # scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1")[1:4]) +
      geom_ribbon(data = subset(subsetDf, Method == "ConcurrentComparator"), aes(ymin = 0, ymax = Density, fill = Method), fill = "White", alpha = 1) +
      geom_line(data = subset(subsetDf, Method == "ConcurrentComparator"), aes(x = Point, y = Density, color = Method), size = 2, alpha = 1, show.legend = FALSE) + #, color = wesanderson::wes_palette("Darjeeling1")[5]) +
      geom_ribbon(data = subset(subsetDf, Method == "ConcurrentComparator"), aes(ymin = 0, ymax = Density, fill = Method), alpha = 0.8) + #, fill = wesanderson::wes_palette("Darjeeling1")[5], alpha = 0.8) + 
      scale_color_manual(values = colorPalette) +
      scale_fill_manual(values = colorPalette, labels = c("CaseControl", "ConcurrentComparator", "HistoricalComparator", "SCCS")) +
      geom_vline(aes(xintercept=0), linetype = "dotted", size = 2, color = "Black") +
      geom_hline(aes(yintercept=0), size = 1.5) +
      ggtitle(exposureName) +
      scale_x_continuous("Risk-ratio (on log-scale)", 
                         breaks = log(breaks), 
                         labels = breaks, 
                         limits = c(log(0.25), log(4))) +
      scale_y_continuous(newDatabaseId,
                         breaks = seq(0.01, densMax, length.out = 5)) +
      coord_cartesian(ylim = c(0, densMax)) +
      theme_minimal() + 
      theme(text = element_text(size=30),
            axis.text.y = element_blank())
    
    # if(databaseId == "optum_ehr" & exposureId == 21216) {
    #   
    #   densPlot <- densPlot + ggtitle("COVID-19 (BNT126b2)") +
    #     scale_y_continuous("Optum EHR",
    #                        breaks = seq(0.01, densMax, length.out = 5),
    #                        labels = seq(0.01, densMax, length.out = 5)) +
    #     coord_cartesian(ylim = c(0,densMax)) +
    #     theme_minimal() +
    #     theme(text = element_text(size=30),
    #           axis.text.y = element_blank())
    #   
    # } else if(databaseId == "optum_ehr" & exposureId == 21217) {
    #   
    #   densPlot <- densPlot + ggtitle("COVID-19 (mRNA-1273)") +
    #     scale_y_continuous("Optum EHR",
    #                        breaks = seq(0.01, densMax, length.out = 5),
    #                        labels = seq(0.01, densMax, length.out = 5)) +
    #     coord_cartesian(ylim = c(0,densMax)) +
    #     theme_minimal() +
    #     theme(text = element_text(size=30),
    #           axis.text.y = element_blank(),
    #           axis.title.y = element_blank())
    #   
    # } else if(databaseId == "truven_mdcd" & exposureId == 21216) {
    #   
    #   densPlot <- densPlot + scale_y_continuous("MDCD", breaks = seq(0.01, densMax, length.out = 5),
    #                                             labels = seq(0.01, densMax, length.out = 5)) +
    #     coord_cartesian(ylim = c(0,densMax)) +
    #     theme_minimal() + 
    #     theme(text = element_text(size=30),
    #           axis.text.y = element_blank())
    #   
    # } else {
    #   
    #   densPlot <- densPlot + scale_y_continuous("MDCD", breaks = seq(0.01, densMax, length.out = 5),
    #                                             labels = seq(0.01, densMax, length.out = 5)) +
    #     coord_cartesian(ylim = c(0,densMax)) +
    #     theme_minimal() + 
    #     theme(text = element_text(size=30),
    #           axis.text.y = element_blank(),
    #           axis.title.y = element_blank()) 
    #     
    #   
    # }
    
    print(densPlot)
    
    sysErrorPlots[[length(sysErrorPlots) + 1]] <- densPlot
    
  }
  
}

## Remove titles from plots 3 and 4

sysErrorPlots[[3]] <- sysErrorPlots[[3]] + theme(plot.title = element_blank())
sysErrorPlots[[4]] <- sysErrorPlots[[4]] + theme(plot.title = element_blank())

## Use patchwork to tie them together

topPlot <- sysErrorPlots[[1]] + sysErrorPlots[[2]]
topPlot <- topPlot & theme(legend.position = "right",
                           legend.text = element_text(size=30),
                           legend.title = element_blank(),
                           axis.title.x = element_blank(), #no x axis in the upper plot
                           axis.title.y = element_text(size=30, angle=90, margin=margin(r=10)))
topPlot <- topPlot + plot_layout(axis_titles = "collect_y",
                                 axes = "collect_y")

print(topPlot)

bottomPlot <- sysErrorPlots[[3]] + sysErrorPlots[[4]]
bottomPlot <- bottomPlot & theme(legend.position = "right",
                                 legend.text = element_text(size=30),
                                 legend.title = element_blank(),
                                 axis.title.x = element_text(size=30),
                                 axis.title.y = element_text(size=30, angle=90, margin=margin(r=10)))
bottomPlot <- bottomPlot + plot_layout(axis_titles = "collect",
                                       axes = "collect_y")

print(bottomPlot)

combinedSysErrorPlot <- (topPlot / bottomPlot) 
combinedSysErrorPlot <- combinedSysErrorPlot + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(combinedSysErrorPlot)

combinedSysErrorPlot <- combinedSysErrorPlot + plot_layout(axis_titles = "collect") 

print(combinedSysErrorPlot)

## Obtain all systematic error distributions for Optum EHR

allPastEstimates <- data.table::fread("E:/Shounak_R/EumaeusAnalysis/allPastEstimates.csv")

databaseId <- "OptumEhr"
exposuresToPlot <- c(21216, 21217, 21215, 211983, 21184, 211833)
exposuresToPlotNames <- c("COVID-19 (BNT126b2)", 
                          "COVID-19 (mRNA-1273)",
                          "Seasonal flu (all)",
                          "Zoster (first or second dose)",
                          "H1N1pdm",
                          "HPV (first or second dose)")

analysisIds <- list("ConcurrentComparator_1-28Days" = 3,
                    "SCCS" = 2,
                    "HistoricalComparator" = 4,
                    "CaseControl" = 2)

oldMethods <- c("SCCS", "HistoricalComparator", "CaseControl")
newMethods <- c("ConcurrentComparator_1-28Days")
allMethods <- c(newMethods, oldMethods)

oldExposures <- exposuresToPlot[!exposuresToPlot %in% c(21216, 21217)]
newExposures <- c(21216, 21217)

lengthGrid <- 100
xseq <- seq(log(0.25), log(4), length.out = lengthGrid)

outcomeIds <- readr::read_csv(system.file("settings/NegativeControls.csv", package="Eumaeus"))
outcomeIds <- outcomeIds$outcomeId

plotInfo <- list()

for(exposureId in exposuresToPlot) {
  
  baseExposureId = as.numeric(substr(exposureId, 1, 5))
  
  for(method in allMethods) {
    
    analysisId = analysisIds[[method]]
    
    if(method == "ConcurrentComparator_1-28Days") {
      
      periodEstimatesFileName <- paste0("E:/Shounak_R/eumaeusTest_optum_ehr_Shounak/",
                                method,
                                "/e_",
                                baseExposureId,
                                "/periodEstimatesWithCvsCalibrated.csv")
      
      periodEstimates <- readr::read_csv(periodEstimatesFileName)
      maxTimePeriod <- max(periodEstimates$seqId)
      
      periodEstimates <- periodEstimates %>% filter(exposureId == !!exposureId,
                                                    analysisId == !!analysisId,
                                                    seqId == !!maxTimePeriod)
      
    } else {
      
      if(exposureId %in% newExposures) {
        
        localMethodName <- ifelse(method == "SCCS", 
                                  "sccs", 
                                  ifelse(method == "HistoricalComparator",
                                         "historicalComparator",
                                         "caseControl"))
        
        periodEstimatesFileName <- paste0("E:/Shounak_R/eumaeusTest_optum_ehr_Shounak/",
                                          localMethodName,
                                          "/e_",
                                          baseExposureId,
                                          "/periodEstimatesWithCvsCalibrated.csv")
        
        periodEstimates <- readr::read_csv(periodEstimatesFileName)
        maxTimePeriod <- max(periodEstimates$seqId)
        
        periodEstimates <- periodEstimates %>% filter(exposureId == !!exposureId,
                                                      analysisId == !!analysisId,
                                                      seqId == !!maxTimePeriod)
        
      } else {
        
        periodEstimates <- allPastEstimates %>% filter(databaseId == "OptumEhr",
                                                       exposureId == !!exposureId,
                                                       analysisId == !!analysisId,
                                                       method == !!method,
                                                       outcomeId %in% outcomeIds)
        
        maxTimePeriod <- max(periodEstimates$periodId)
        
        periodEstimates <- periodEstimates %>% filter(periodId == !!maxTimePeriod)
        
      }
      
    }
    
    ## Fit null distribution
    
    fitNull <- EmpiricalCalibration::fitNull(periodEstimates$logRr,
                                             periodEstimates$seLogRr)
    
    if(fitNull[2] <= 0.01) {
      
      fitNull <- EmpiricalCalibration::fitMcmcNull(periodEstimates$logRr,
                                                   periodEstimates$seLogRr)
      
    }
    
    unitDf <- data.frame("Point" = xseq)
    unitDf$exposureId <- exposureId
    unitDf$Method <- method
    unitDf$analysisId <- analysisId
    
    if(is(fitNull, "null")) {
      
      unitDf$Density <- dnorm(xseq, fitNull[1], fitNull[2])
      
    } else {
      
      unitDf$Density <- dnorm(xseq, fitNull[1], 1 / sqrt(fitNull[2]))
      
    }
    
    plotInfo[[length(plotInfo) + 1]] <- unitDf
    
  }
  
}

plotInfo <- bind_rows(plotInfo)
write.csv(plotInfo, "E:/Shounak_R/EumaeusAnalysis/nullDistributionsOptumEHR.csv")

# Now obtain plots using plotInfo

allPlots <- list()

for(exposureId in exposuresToPlot) {
  
  exposureName <- exposuresToPlotNames[which(exposuresToPlot == exposureId)]
  
  subsetDf <- plotInfo %>% filter(exposureId == !!exposureId)
  subsetDf$Method[subsetDf$Method == "ConcurrentComparator_1-28Days"] = "ConcurrentComparator"
  
  pl <- ggplot(subsetDf, aes(x = Point, group = Method)) +
    geom_line(data = subset(subsetDf, Method != "ConcurrentComparator"), aes(x = Point, y = Density, color = Method), size = 2, alpha = 1, show.legend=FALSE) +
    geom_ribbon(data = subset(subsetDf, Method != "ConcurrentComparator"), aes(ymin = 0, ymax = Density, fill = Method), alpha = 0.8) +
    # scale_color_manual(values = wesanderson::wes_palette("Darjeeling1")[1:4]) +
    # scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1")[1:4]) +
    geom_ribbon(data = subset(subsetDf, Method == "ConcurrentComparator"), aes(ymin = 0, ymax = Density, fill = Method), fill = "White", alpha = 1) +
    geom_line(data = subset(subsetDf, Method == "ConcurrentComparator"), aes(x = Point, y = Density, color = Method), size = 2, alpha = 1, show.legend = FALSE) + #, color = wesanderson::wes_palette("Darjeeling1")[5]) +
    geom_ribbon(data = subset(subsetDf, Method == "ConcurrentComparator"), aes(ymin = 0, ymax = Density, fill = Method), alpha = 0.8) + #, fill = wesanderson::wes_palette("Darjeeling1")[5], alpha = 0.8) + 
    scale_color_manual(values = colorPalette) +
    scale_fill_manual(values = colorPalette, labels = c("CaseControl", "ConcurrentComparator", "HistoricalComparator", "SCCS")) +
    geom_vline(aes(xintercept=0), linetype = "dotted", size = 2, color = "Black") +
    geom_hline(aes(yintercept=0), size = 1.5) +
    ggtitle(exposureName) +
    scale_x_continuous("Risk-ratio (on log-scale)", 
                       breaks = log(breaks), 
                       labels = breaks, 
                       limits = c(log(0.25), log(4))) +
    scale_y_continuous("Density",
                       breaks = c(0, densMax),
                       labels = c(0, densMax)) +
    coord_cartesian(ylim = c(0, densMax)) +
    theme_minimal() + 
    theme(text = element_text(size=30))
  
  print(pl)
  
  allPlots[[length(allPlots) + 1]] <- pl
  
}

## Now arrange them in patchwork

densPlot <- wrap_plots(allPlots, nrow = 3, ncol = 2) &
  theme(legend.position = "bottom",
        legend.text = element_text(size=30),
        legend.title = element_blank(),
        axis.title.x = element_text(size=30),
        axis.title.y = element_text(size=30, angle=90, margin=margin(r=10))) 

densPlot <- densPlot +
  plot_annotation(title = "Systematic error across exposures",
                  theme = theme(plot.title = element_text(size = 40, hjust = 0.5, margin=margin(b=20))))

densPlot <- densPlot +
  plot_layout(guides = "collect", 
              axis_titles = "collect",
              axes="collect_y")

print(densPlot)

ggsave(densPlot,
       filename = "E:/Shounak_R/Eumaeus/extras/finalPlots/sysErrorOptumEHR.eps",
       width=450,
       height=550,
       units="mm",
       dpi = 400)

Cairo::CairoPS("E:/Shounak_R/Eumaeus/extras/finalPlots/sysErrorOptumEHRCairoPS.eps", width=450, 
               height = 550, units = "mm", dpi = 400)
print(densPlot)
dev.off()

#### Variation of type 1 error and power across data sources
#### Pick Optum EHR, MDCD * 2 covid vaccines

source("E:/Shounak_R/Eumaeus/extras/calibrationCodes/calibrateLLR/type1ErrorPowerPlots.R")

componentPlotsType1Error <- list()
componentPlotsPower <- list()

for(databaseId in c("OptumEhr", "IBM_MDCD")) {
  
  for(exposureId in c(21216, 21217)) {
    
    exposureName <- exposuresToPlotNames[which(exposuresToPlot == exposureId)]
    
    yTextVanish <- ifelse(exposureId == 21216, 0, 1)
    yTextVanish <- as.logical(yTextVanish)
    
    pl <- plotType1ErrorPower(databaseId,
                              exposureId,
                              allResults,
                              analysisIds,
                              exposureName,
                              yVanish=TRUE,
                              yTextVanish)
    
    componentPlotsType1Error[[length(componentPlotsType1Error) + 1]] <- pl$Type1ErrorCalibrated
    componentPlotsPower[[length(componentPlotsPower) + 1]] <- pl$Power2
    
  }
  
}

## patchwork stuff

combinedPlot <- function(plotList, power=TRUE) {
  
  if(power==TRUE) {
    
    breaks = seq(0,1,by=0.25)
    #limit = 1
    
  } else {
    
    breaks = c(0, 0.05, seq(0.1, 0.2, by = 0.05))
    #limit = 0.2
    
  }
  
  limit = max(breaks)
  
  plotList[[1]] <- plotList[[1]] + scale_y_continuous("Optum EHR", breaks = breaks, limits = c(0,limit))
  plotList[[2]] <- plotList[[2]] + scale_y_continuous("Optum EHR", breaks = breaks, limits = c(0, limit))
  plotList[[3]] <- plotList[[3]] + scale_y_continuous("MDCD", breaks = breaks, limits = c(0, limit))
  plotList[[4]] <- plotList[[4]] + scale_y_continuous("MDCD", breaks = breaks, limits = c(0, limit))
  
  #bottom two subfigures have no titles
  plotList[[3]] <- plotList[[3]] + theme(plot.title = element_blank())
  plotList[[4]] <- plotList[[4]] + theme(plot.title = element_blank())
  
  ## Use patchwork to tie them together
  
  topPlot <- plotList[[1]] + plotList[[2]]
  topPlot <- topPlot & theme(legend.position = "right",
                             legend.text = element_text(size=30),
                             legend.title = element_blank(),
                             axis.title.x = element_blank(), #no x axis in the upper plot
                             axis.title.y = element_blank(),
                             axis.text.x = element_blank())
  topPlot <- topPlot + plot_layout(axes = "collect_y",
                                   axis_titles = "collect_y")
  topPlot <- topPlot & theme(axis.title.y = element_blank())
  topPlot <- topPlot & theme(axis.title.y = element_text(size=30, angle=90, margin=margin(r=10)))
  
  #print(topPlot)
  
  bottomPlot <- plotList[[3]] + plotList[[4]]
  bottomPlot <- bottomPlot & theme(legend.position = "right",
                                   legend.text = element_text(size=30),
                                   legend.title = element_blank(),
                                   axis.title.x = element_text(size=30),
                                   axis.title.y = element_text(size=30, angle=90, margin=margin(r=10)))
  bottomPlot <- bottomPlot + plot_layout(axis_titles = "collect",
                                         axes = "collect_y") & xlab("Months")
  
  #print(bottomPlot)
  
  combinedPlot <- (topPlot / bottomPlot) 
  combinedPlot <- combinedPlot + plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  print(combinedPlot)
  
  return(combinedPlot)
  
}

combinedT1EPlot <- combinedPlot(componentPlotsType1Error, power=FALSE)
combinedPowerPlot <- combinedPlot(componentPlotsPower)

ggsave(combinedT1EPlot,
       filename = "E:/Shounak_R/Eumaeus/extras/finalPlots/T1EAcrossDataSources.eps",
       width=425,
       height=205,
       units="mm",
       dpi = 400)

ggsave(combinedPowerPlot,
       filename = "E:/Shounak_R/Eumaeus/extras/finalPlots/PowerAcrossDataSources.eps",
       width=425,
       height=205,
       units="mm",
       dpi = 400)

# componentPlotsType1Error[[1]] <- componentPlotsType1Error[[1]] + scale_y_continuous("Optum EHR", breaks = c(0, 0.05, seq(0.1, 0.2, by=0.05)), limits = c(0, 0.2))
# componentPlotsType1Error[[2]] <- componentPlotsType1Error[[2]] + scale_y_continuous("Optum EHR", breaks = c(0, 0.05, seq(0.1, 0.2, by=0.05)), limits = c(0, 0.2))
# componentPlotsType1Error[[3]] <- componentPlotsType1Error[[3]] + scale_y_continuous("MDCD", breaks = c(0, 0.05, seq(0.1, 0.2, by=0.05)), limits = c(0, 0.2))
# componentPlotsType1Error[[4]] <- componentPlotsType1Error[[4]] + scale_y_continuous("MDCD", breaks = c(0, 0.05, seq(0.1, 0.2, by=0.05)), limits = c(0, 0.2))
# 
# #bottom two subfigures have no titles
# componentPlotsType1Error[[3]] <- componentPlotsType1Error[[3]] + theme(plot.title = element_blank())
# componentPlotsType1Error[[4]] <- componentPlotsType1Error[[4]] + theme(plot.title = element_blank())
# 
# ## Use patchwork to tie them together
# 
# topPlot <- componentPlotsType1Error[[1]] + componentPlotsType1Error[[2]]
# topPlot <- topPlot & theme(legend.position = "right",
#                            legend.text = element_text(size=30),
#                            legend.title = element_blank(),
#                            axis.title.x = element_blank(), #no x axis in the upper plot
#                            axis.title.y = element_blank(),
#                            axis.text.x = element_blank())
# topPlot <- topPlot + plot_layout(axes = "collect_y",
#                                  axis_titles = "collect_y")
# topPlot <- topPlot & theme(axis.title.y = element_blank())
# topPlot <- topPlot & theme(axis.title.y = element_text(size=30, angle=90, margin=margin(r=10)))
# 
# print(topPlot)
# 
# bottomPlot <- componentPlotsType1Error[[3]] + componentPlotsType1Error[[4]]
# bottomPlot <- bottomPlot & theme(legend.position = "right",
#                                  legend.text = element_text(size=30),
#                                  legend.title = element_blank(),
#                                  axis.title.x = element_text(size=30),
#                                  axis.title.y = element_text(size=30, angle=90, margin=margin(r=10)))
# bottomPlot <- bottomPlot + plot_layout(axis_titles = "collect",
#                                        axes = "collect_y") & xlab("Months")
# 
# print(bottomPlot)
# 
# combinedT1EPlot <- (topPlot / bottomPlot) 
# combinedT1EPlot <- combinedT1EPlot + plot_layout(guides = "collect") &
#   theme(legend.position = "bottom")
# 
# print(combinedT1EPlot)


