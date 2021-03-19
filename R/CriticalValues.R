# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of Eumaeus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


computeCriticalValues <- function(outputFolder, maxCores) {
  cluster <- ParallelLogger::makeCluster(min(10, maxCores))
  ParallelLogger::clusterRequire(cluster, "dplyr")
  on.exit(ParallelLogger::stopCluster(cluster))
  
  fileName <- file.path(outputFolder, "hcSummary_withCvs.csv")
  if (!file.exists(fileName)) {
    ParallelLogger::logInfo("- Computing critical values for historic comparator")
    hcEstimates <- loadEstimates(file.path(outputFolder, "hcSummary.csv")) 
    subsets <- split(hcEstimates, paste(hcEstimates$analysisId, hcEstimates$exposureId, hcEstimates$outcomeId))
    cvs <- ParallelLogger::clusterApply(cluster, subsets, computeHistoricalComparatorCv)
    cvs <- bind_rows(cvs)
    rm(subsets)
    hcEstimates <- hcEstimates %>%
      inner_join(cvs, by = c("analysisId", "exposureId", "outcomeId"))
    readr::write_csv(hcEstimates, fileName)
  }
  
  fileName <- file.path(outputFolder, "ccSummary_withCvs.csv")
  if (!file.exists(fileName)) {
    ParallelLogger::logInfo("- Computing critical values for case-control")
    ccEstimates <- loadEstimates(file.path(outputFolder, "ccSummary.csv")) 
    subsets <- split(ccEstimates, paste(ccEstimates$analysisId, ccEstimates$exposureId, ccEstimates$outcomeId))
    cvs <- ParallelLogger::clusterApply(cluster, subsets, Eumaeus:::computeCaseControlCv)
    cvs <- bind_rows(cvs)
    rm(subsets)
    ccEstimates <- ccEstimates %>%
      inner_join(cvs, by = c("analysisId", "exposureId", "outcomeId"))
    readr::write_csv(ccEstimates, fileName)
  }
  
  fileName <- file.path(outputFolder, "cmSummary_withCvs.csv")
  if (!file.exists(fileName)) {
    ParallelLogger::logInfo("- Computing critical values for cohort method")
    cmEstimates <- loadEstimates(file.path(outputFolder, "cmSummary.csv")) 
    subsets <- split(cmEstimates, paste(cmEstimates$analysisId, cmEstimates$exposureId, cmEstimates$outcomeId))
    cvs <- ParallelLogger::clusterApply(cluster, subsets, computeCohortMethodCv)
    cvs <- bind_rows(cvs)
    rm(subsets)
    cmEstimates <- cmEstimates %>%
      inner_join(cvs, by = c("analysisId", "exposureId", "outcomeId"))
    readr::write_csv(cmEstimates, fileName)
  }
  
  fileName <- file.path(outputFolder, "sccsSummary_withCvs.csv")
  if (!file.exists(fileName)) {
    ParallelLogger::logInfo("- Computing critical values for SCCS / SCRI")
    sccsstimates <- loadEstimates(file.path(outputFolder, "sccsSummary.csv")) 
    subsets <- split(sccsstimates, paste(sccsstimates$analysisId, sccsstimates$exposureId, sccsstimates$outcomeId))
    cvs <- ParallelLogger::clusterApply(cluster, subsets, computeSccsCv)
    cvs <- bind_rows(cvs)
    rm(subsets)
    sccsstimates <- sccsstimates %>%
      inner_join(cvs, by = c("analysisId", "exposureId", "outcomeId"))
    readr::write_csv(sccsstimates, fileName)
  }
}

computeCaseControlCv <- function(subset) {
  # subset <- subsets[[1]]
  sampleSizeUpperLimit <- max(subset$cases, na.rm = TRUE)
  if (sampleSizeUpperLimit <= 5) {
    cv <- NA
  } else {
    cases <- subset %>%
      arrange(.data$seqId) %>%
      pull(.data$cases)
    looks <- length(cases)
    if (looks > 1) {
      cases[2:looks] <- cases[2:looks] - cases[1:(looks-1)]
      cases <- cases[cases != 0]
    }
    cv <- Eumaeus:::computeTruncatedBinomialCv(n = sampleSizeUpperLimit,
                                               z = max(subset$controls) / max(subset$cases),
                                               groupSizes = cases)
  }
  return(tibble(analysisId = subset$analysisId[1],
                exposureId = subset$exposureId[1],
                outcomeId = subset$outcomeId[1],
                criticalValue = cv))
}

computeCohortMethodCv <- function(subset) {
  # subset <- subsets[[1]]
  # subset <- cmEstimates[cmEstimates$analysisId == 10 & cmEstimates$exposureId == 211841 & cmEstimates$outcomeId == 10703, ]
  subset$events <- subset$eventsTarget + subset$eventsComparator
  sampleSizeUpperLimit <- max(subset$events, na.rm = TRUE)
  if (sampleSizeUpperLimit <= 5) {
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
    cv <- Eumaeus:::computeTruncatedBinomialCv(n = sampleSizeUpperLimit,
                                               z = max(subset$comparatorDays) / max(subset$targetDays),
                                               groupSizes = events)
  }
  return(tibble(analysisId = subset$analysisId[1],
                exposureId = subset$exposureId[1],
                outcomeId = subset$outcomeId[1],
                criticalValue = cv))
}

computeSccsCv <- function(subset) {
  # subset <- subsets[[1]]
  sampleSizeUpperLimit <- max(subset$outcomeEvents , na.rm = TRUE)
  if (sampleSizeUpperLimit <= 5) {
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
    cv <- Eumaeus:::computeTruncatedBinomialCv(n = sampleSizeUpperLimit,
                                               z = max(subset$daysObserved - subset$exposedDays) / max(subset$exposedDays),
                                               groupSizes = events)
  }
  return(tibble(analysisId = subset$analysisId[1],
                exposureId = subset$exposureId[1],
                outcomeId = subset$outcomeId[1],
                criticalValue = cv))
}



computeHistoricalComparatorCv <- function(subset) {
  # subset <- subsets[[2]]
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
  if (sampleSizeUpperLimit <= 5) {
    cv <- NA
  } else {
    cv <- Eumaeus:::computeTruncatedPoissonCv(n = sampleSizeUpperLimit,
                                              groupSizes = expectedOutcomes)
  }
  return(tibble(analysisId = subset$analysisId[1],
                exposureId = subset$exposureId[1],
                outcomeId = subset$outcomeId[1],
                criticalValue = cv))
}

computeTruncatedBinomialCv <- function(n, z, groupSizes) {
  if (n > 250) {
    groupSizes <- round(groupSizes * 250 / n)
    n <- sum(groupSizes)
  }
  cv <- Sequential::CV.Binomial(N = n,
                                M = 1,
                                alpha = 0.05,
                                z = z,
                                GroupSizes = groupSizes)$cv
  return(cv)
}

computeTruncatedPoissonCv <- function(n, groupSizes) {
  if (n > 250) {
    groupSizes <- round(groupSizes * 250 / n)
    n <- sum(groupSizes)
  }
  cv <- Sequential::CV.Poisson(SampleSize = n,
                               alpha = 0.05,
                               M = 1,
                               GroupSizes = groupSizes)
  return(cv)
}
