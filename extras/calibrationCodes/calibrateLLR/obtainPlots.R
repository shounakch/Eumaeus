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
                    "SCCS" = 3,
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

