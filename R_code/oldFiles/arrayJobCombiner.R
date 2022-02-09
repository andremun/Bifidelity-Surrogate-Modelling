combineArrayResults <- function(runName, arrayStart, arrayEnd, jobsPerArray = 1){
  if(jobsPerArray == 1){combinedData <- read.table(paste0("../data/clusterResults/", runName, "_arrayJob", arrayStart, ".txt"), header = TRUE, sep = " ", fill = TRUE)}
  else{combinedData <- read.table(paste0("../data/clusterResults/", runName, "_arrayJob", (arrayStart - 1) * jobsPerArray + 1, "-", arrayStart * jobsPerArray, ".txt"), header = TRUE, sep = " ", fill = TRUE)}
  for(i in (arrayStart + 1):arrayEnd){
    if(jobsPerArray == 1){filename <- paste0("../data/clusterResults/", runName, "_arrayJob", i, ".txt")}
    else{filename <- paste0("../data/clusterResults/", runName, "_arrayJob", (i - 1) * jobsPerArray + 1, "-", i * jobsPerArray, ".txt")}
    
    if(!file.exists(filename)){
      print(paste0("Skipping file ", filename, " as it does not exist!"))
      next
    }
    newData <- read.table(filename, header = TRUE, sep = " ", fill = TRUE)
    combinedData <- rbind(combinedData, newData)
  }
  return(combinedData)
}

processLocalCorrMean <- function(localCorrs){
  # Get first set of bins
  # print(localCorrs)
  #print(paste0("Have ", length(localCorrs), "bins"))
  bins <- as.numeric(strsplit(gsub('[() ]', '', as.character(localCorrs)), ";")[[1]])
  nBins <- length(bins)
  data <- data.frame(t(bins))
  if(length(localCorrs) >= 2){
    for(i in 2:length(localCorrs)){
      bins <- as.numeric(strsplit(gsub('[() ]', '', as.character(localCorrs)), ";")[[i]])
      if(length(bins) < nBins){print(paste0("Skipping bin as it is too short ", bins))}
      data[i, ] <- t(bins)
    }
  }
  returnString <- "("
  for(i in 1:(ncol(data)-1)){
    returnString <- paste0(returnString, mean(data[, i]), ";")
  }
  returnString <- paste0(returnString, mean(data[, ncol(data)]), ")")
  return(returnString)
}


averagePerformance <- function(data){
  # Now get averages
  modes <- unique(data$sampleStrategy)
  techniques <- unique(data$technique)
  functions <- unique(data$instance)
  budgets <-unique(data$budget)
  initialHighBudgets <- unique(data$initialBudgetHigh)
  initialLowBudgets <- unique(data$initialBudgetLow)
  costRatios <- unique(data$costRatio) 
  
  modesVals <- c()
  techniquesVals <- c()
  functionsVals <-c()
  budgetsVals <- c()
  initialHighBudgetsVals <- c()
  initialLowBudgetsVals <- c()
  costRatiosVals <- c()
  performance <- c()
  performanceVar <- c()
  globalCorr <- c()
  lowFiCorr <- c()
  highFiCorr <- c()
  err <- c()
  localCorr <- c()
  
  for(mode in modes){
    for(technique in techniques){
      for(func in functions){
        for(budget in budgets){
          for(initialHighBudget in initialHighBudgets){
            for(initialLowBudget in initialLowBudgets){
              for(costRatio in costRatios){
                # First check this actually exists
                if(length(data[data$sampleStrategy == mode &
                               data$technique == technique & 
                               data$instance == func & 
                               data$budget == budget & 
                               data$initialBudgetHigh == initialHighBudget & 
                               data$initialBudgetLow == initialLowBudget & 
                               data$costRatio == costRatio, c("performance")]) == 0){next}
                modesVals <- c(modesVals, mode)
                techniquesVals <- c(techniquesVals, technique)
                functionsVals <- c(functionsVals, func)
                budgetsVals <- c(budgetsVals, budget)
                initialHighBudgetsVals <- c(initialHighBudgetsVals, initialHighBudget)
                initialLowBudgetsVals <- c(initialLowBudgetsVals, initialLowBudget)
                costRatiosVals <- c(costRatiosVals, costRatio)
                
                performance <- c(performance, mean(data[data$sampleStrategy == mode &
                                                          data$technique == technique & 
                                                          data$instance == func & 
                                                          data$budget == budget & 
                                                          data$initialBudgetHigh == initialHighBudget & 
                                                          data$initialBudgetLow == initialLowBudget & 
                                                          data$costRatio == costRatio, c("performance")]))
                
                performanceVar <- c(performanceVar, sqrt(var(data[data$sampleStrategy == mode &
                                               data$technique == technique & 
                                               data$instance == func & 
                                               data$budget == budget & 
                                               data$initialBudgetHigh == initialHighBudget & 
                                               data$initialBudgetLow == initialLowBudget & 
                                               data$costRatio == costRatio, c("performance")])))
                
                globalCorr <- c(globalCorr, mean(data[data$sampleStrategy == mode &
                                                          data$technique == technique & 
                                                          data$instance == func & 
                                                          data$budget == budget & 
                                                          data$initialBudgetHigh == initialHighBudget & 
                                                          data$initialBudgetLow == initialLowBudget & 
                                                          data$costRatio == costRatio, c("globalCorrelation")]))
                
                lowFiCorr <- c(lowFiCorr, mean(data[data$sampleStrategy == mode &
                                                        data$technique == technique & 
                                                        data$instance == func & 
                                                        data$budget == budget & 
                                                        data$initialBudgetHigh == initialHighBudget & 
                                                        data$initialBudgetLow == initialLowBudget & 
                                                        data$costRatio == costRatio, c("lowFiLowCorrelation")]))
                
                highFiCorr <- c(highFiCorr, mean(data[data$sampleStrategy == mode &
                                                      data$technique == technique & 
                                                      data$instance == func & 
                                                      data$budget == budget & 
                                                      data$initialBudgetHigh == initialHighBudget & 
                                                      data$initialBudgetLow == initialLowBudget & 
                                                      data$costRatio == costRatio, c("highFiLowCorrelation")]))
                
                err <- c(err, mean(data[data$sampleStrategy == mode &
                                                      data$technique == technique & 
                                                      data$instance == func & 
                                                      data$budget == budget & 
                                                      data$initialBudgetHigh == initialHighBudget & 
                                                      data$initialBudgetLow == initialLowBudget & 
                                                      data$costRatio == costRatio, c("relativeError")]))
                
                
                localCorr <- c(localCorr, processLocalCorrMean(data[data$sampleStrategy == mode &
                                          data$technique == technique & 
                                          data$instance == func & 
                                          data$budget == budget & 
                                          data$initialBudgetHigh == initialHighBudget & 
                                          data$initialBudgetLow == initialLowBudget & 
                                          data$costRatio == costRatio, c("localCorrelation")]))
              }
            }
          }
        }
      }
    }
  }
  averaged <- data.frame("sampleStrategy" = modesVals,
                         "technique" = techniquesVals,
                         "instance" = functionsVals,
                         "budget" = budgetsVals,
                         "initialBudgetHigh" = initialHighBudgetsVals,
                         "initialBudgetLow" = initialLowBudgetsVals,
                         "costRatio" = costRatiosVals,
                         "performance" = performance,
                         "performanceVariance" = performanceVar,
                         "globalCorrelation" = globalCorr,
                         "lowFiLowCorrelation" = lowFiCorr,
                         "highFiLowCorrelation" = highFiCorr,
                         "relativeError" = err,
                         "localCorrelation" = localCorr)
  
  return(averaged)
}


getAveragePerformance <- function(runName, arrayStart, arrayEnd, jobsPerArray = 1){
  data <- combineArrayResults(runName, arrayStart, arrayEnd, jobsPerArray)
  averagePerformance(data)
}

getAveragePerformanceWithAnalysis <- function(runName, arrayStart, arrayEnd, jobsPerArray = 1){
  data <- combineArrayResults(runName, arrayStart, arrayEnd, jobsPerArray)
  averaged <- averagePerformance(data)
  for(inst in unique(averaged$instance)){
    #print(as.character(inst))
    # Grab data to compare
    krigVals <- data[data$instance == inst & data$technique == "kriging" & !is.na(data$performance), "performance"]
    cokrigVals <- data[data$instance == inst & data$technique == "cokriging" & !is.na(data$performance), "performance"]
    #print(krigVals)
    #print(cokrigVals)
    if(length(krigVals) != length(cokrigVals)){next}
    # Test hypothesis that the performance is the same
    hypSame <- wilcox.test(krigVals, cokrigVals)
    hypKrigLower <- wilcox.test(cokrigVals, krigVals, alternative = "l")
    hypCoKrigLower <- wilcox.test(krigVals, cokrigVals, alternative = "l")
    #print(hypSame$p.value)
    #print(hypKrigLower$p.value)
    #print(hypCoKrigLower$p.value)
    
    # Store this info
    averaged[averaged$instance == inst & averaged$technique == "kriging", "willcoxKrigGood"] <- hypSame$p.value > 0.05 | hypKrigLower$p.value > 0.05
    averaged[averaged$instance == inst & averaged$technique == "cokriging", "willcoxCoKrigGood"] <- hypSame$p.value > 0.05 | hypCoKrigLower$p.value > 0.05
  }
  return(averaged)
}

# performAnalysis <- function(data, averaged, start, end){
#   count <- 0
#   for(inst in unique(averaged$instance)){
#     count <- count + 1
#     if(count < start | count > end){next}
#     print(as.character(inst))
#     # Grab data to compare
#     krigVals <- data[data$instance == inst & data$technique == "kriging" & !is.na(data$performance), "performance"]
#     cokrigVals <- data[data$instance == inst & data$technique == "cokriging" & !is.na(data$performance), "performance"]
#     #print(krigVals)
#     #print(cokrigVals)
#     if(length(krigVals) != length(cokrigVals)){next}
#     #if(inst == "COCOfunction17-dim1-seed1-globalNoise10-centres9-radius0.1-noise6-amp0.1"){next}
#     # Test hypothesis that the performance is the same
#     hypSame <- wilcox.test(krigVals, cokrigVals)
#     hypKrigLower <- wilcox.test(cokrigVals, krigVals, alternative = "l")
#     hypCoKrigLower <- wilcox.test(krigVals, cokrigVals, alternative = "l")
#     print(hypSame$p.value)
#     print(hypKrigLower$p.value)
#     print(hypCoKrigLower$p.value)
#     
#     # Store this info
#     averaged[averaged$instance == inst & averaged$technique == "kriging", "willcoxKrigGood"] <- hypSame$p.value > 0.05 | hypKrigLower$p.value > 0.05
#     averaged[averaged$instance == inst & averaged$technique == "cokriging", "willcoxCoKrigGood"] <- hypSame$p.value > 0.05 | hypCoKrigLower$p.value > 0.05
#   }
#   return(averaged)
# }


# Example usage
# averaged <- getAveragePerformance("customSetup", 1, 80)
# combined <- combineArrayResults("customSetup", 1, 80)
