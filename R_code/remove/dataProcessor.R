calculate_local_probabilites <- function(localCorrs, corrValues){
  processedData <- data.frame(matrix(ncol = (4 + 4 * length(corrValues)), nrow = 0))
  for(i in 1:length(localCorrs)){
    bins <- as.numeric(strsplit(gsub('[() ]', '', as.character(localCorrs[[i]])), ";")[[1]])
    width <- 2.0 / length(bins)
    limit <- -1
    # Initialise values
    mean <- 0
    squaredMean <- 0
    meanR2 <- 0
    squaredMeanR2 <- 0
    corrs <- rep(0, 4 * length(corrValues))
    
    for(j in 1:length(bins)){
      midPoint <- limit + width / 2
      # First deal with mean values
      mean <- mean + midPoint * bins[j]
      squaredMean <- squaredMean + midPoint * midPoint * bins[j]
      meanR2 <- meanR2 + midPoint * midPoint * bins[j]
      squaredMeanR2 <- squaredMeanR2 + midPoint * midPoint * midPoint * midPoint *bins[j]
      
      # Now can deal with local correlations
      for(k in 1:length(corrValues)){
        if(abs(limit) > corrValues[[k]] || abs(abs(limit) - corrValues[[k]]) < 0.001){corrs[[k]] <- corrs[[k]] + bins[j]}
        if((limit)^2 > corrValues[[k]] || abs((limit)^2 - corrValues[[k]]) < 0.001){corrs[[k + length(corrValues)]] <- corrs[[k + length(corrValues)]] + bins[j]}
        if(limit > corrValues[[k]] || abs(limit - corrValues[[k]]) < 0.001){corrs[[k + 2*length(corrValues)]] <- corrs[[k + 2*length(corrValues)]] + bins[j]}
        if(-(limit) > corrValues[[k]] || abs(-(limit) - corrValues[[k]]) < 0.001){corrs[[k + 3*length(corrValues)]] <- corrs[[k + 3*length(corrValues)]] + bins[j]}
      }
      limit <- limit + width
    }
    processedData[i, ] <- c(sqrt(abs(squaredMean - mean * mean)), 
                            sqrt(abs(squaredMean - mean * mean)) / abs(mean),
                            sqrt(abs(squaredMeanR2 - meanR2 * meanR2)), 
                            sqrt(abs(squaredMeanR2 - meanR2 * meanR2)) / abs(meanR2),
                            corrs)
  }
  
  colNames <- c("localCorrelationSD", "localCorrelationCoeffVariation", "localCorrelationSD_R2", "localCorrelationCoeffVariation_R2", rep("", 4*length(corrValues)))
  for(k in 1:length(corrValues)){
    colNames[[4 + k + 0*length(corrValues)]] <- paste0("localCorrAbs_", corrValues[[k]])
    colNames[[4 + k + 1*length(corrValues)]] <- paste0("localCorrR2_", corrValues[[k]])
    colNames[[4 + k + 2*length(corrValues)]] <- paste0("localCorr_", corrValues[[k]])
    colNames[[4 + k + 3*length(corrValues)]] <- paste0("localCorrNeg_", corrValues[[k]])
  }
  colnames(processedData) <- colNames
  return(processedData)
}
# 
# calculate_local_probability_coefficient_of_variation <- function(localCorrs){
#   col <- vector(mode = "numeric", length(localCorrs))
#   for(i in 1:length(localCorrs)){
#     #print(paste0(name, "prob", prob, "-", i, "/", nrow(data)))
#     bins <- as.numeric(strsplit(gsub('[() ]', '', as.character(localCorrs[[i]])), ";")[[1]])
#     # Caluclate width of bins
#     width <- 2.0 / length(bins)
#     limit <- -1
#     mean <- 0
#     squaredMean <- 0
#     for(j in 1:length(bins)){
#       midPoint <- limit + width / 2
#       mean <- mean + midPoint * bins[j]
#       squaredMean <- squaredMean + midPoint * midPoint * bins[j]
#       limit <- limit + width
#     }
#     # This should always work, but get floating point error if deviation is 0, so take absolute
#     col[[i]] <- sqrt(abs(squaredMean - mean * mean)) / abs(mean)
#   }
#   return(col)
# }
# 
# calculate_local_probability_variation <- function(localCorrs){
#   col <- vector(mode = "numeric", length(localCorrs))
#   for(i in 1:length(localCorrs)){
#     #print(paste0(name, "prob", prob, "-", i, "/", nrow(data)))
#     bins <- as.numeric(strsplit(gsub('[() ]', '', as.character(localCorrs[[i]])), ";")[[1]])
#     # Caluclate width of bins
#     width <- 2.0 / length(bins)
#     limit <- -1
#     mean <- 0
#     squaredMean <- 0
#     for(j in 1:length(bins)){
#       midPoint <- limit + width / 2
#       mean <- mean + midPoint * bins[j]
#       squaredMean <- squaredMean + midPoint * midPoint * bins[j]
#       limit <- limit + width
#     }
#     # This should always work, but get floating point error if deviation is 0, so take absolute
#     col[[i]] <- sqrt(abs(squaredMean - mean * mean))
#   }
#   return(col)
# }
# 
# calculate_local_probability_neg <- function(localCorrs, prob){
#   col <- vector(mode = "numeric", length(localCorrs))
#   for(i in 1:length(localCorrs)){
#     #print(paste0(name, "prob", prob, "-", i, "/", nrow(data)))
#     bins <- as.numeric(strsplit(gsub('[() ]', '', as.character(localCorrs[[i]])), ";")[[1]])
#     # Caluclate width of bins
#     width <- 2.0 / length(bins)
#     limit <- -1
#     probSum <- 0
#     for(j in 1:length(bins)){
#       limit <- limit + width
#       if(-limit < prob && abs(-limit - prob) > 0.001){break}
#       probSum <- probSum + bins[j]
#     }
#     col[[i]] <- probSum
#   }
#   return(col)
# }
# 
# calculate_local_probability <- function(localCorrs, prob){
#   col <- vector(mode = "numeric", length(localCorrs))
#   for(i in 1:length(localCorrs)){
#     #print(paste0(name, "prob", prob, "-", i, "/", nrow(data)))
#     bins <- as.numeric(strsplit(gsub('[() ]', '', as.character(localCorrs[[i]])), ";")[[1]])
#     # Caluclate width of bins
#     width <- 2.0 / length(bins)
#     limit <- 1
#     probSum <- 0
#     for(j in length(bins):1){
#       limit <- limit - width
#       if(limit < prob && abs(limit - prob) > 0.001){break}
#       probSum <- probSum + bins[j]
#     }
#     col[[i]] <- probSum
#   }
#   return(col)
# }
# 
# calculate_local_probability_absolute <- function(localCorrs, prob){
#   return(calculate_local_probability(localCorrs, prob) + calculate_local_probability_neg(localCorrs, prob))
# }




dataWithSimpleFeatures <- function(data, localCorrs){
  colNum <- 12 + 4*length(localCorrs)
  processedData <- data.frame(matrix(ncol = colNum, nrow = 0))
  entries <- 0
  total <- length(unique(data$instance))
  for(instance in unique(data$instance)){
    cat("\rWorking on instance ", entries + 1, "/", length(unique(data$instance)))
    # print(paste0("Working on row ", entries, " out of ", total))
    # First entry should be identical to the rest, do a check for the first seed
    firstSeed = data[data$instance == instance, "seed"][[1]]
    if(length(unique(data[data$instance == instance & data$seed == firstSeed, "globalCorrelation"])) > 1){
      print(paste0("Instance ", instance, " has something weird, same seeds disagree for globalCorr. Skipping this one."))
      next
    }
    entries <- entries + 1
    # If there is a problem, something to deal with. Either way, want to look at unique vals and all seeds
    interest <- data[data$instance == instance &
                       data$sampleStrategy == unique(data[data$instance == instance, "sampleStrategy"])[[1]] &
                       data$technique == unique(data[data$instance == instance, "technique"])[[1]] &
                       data$budget == unique(data[data$instance == instance, "budget"])[[1]] &
                       data$initialBudgetHigh == unique(data[data$instance == instance, "initialBudgetHigh"])[[1]] &
                       data$initialBudgetLow == unique(data[data$instance == instance, "initialBudgetLow"])[[1]] &
                       data$costRatio == unique(data[data$instance == instance, "costRatio"])[[1]], ]
    
    vals <- c(instance, 
              mean(as.double(interest$globalCorrelation)),
              mean((as.double(interest$globalCorrelation))^2),
              mean(interest$lowFiLowCorrelation), 
              mean((interest$lowFiLowCorrelation)^2), 
              mean(interest$highFiLowCorrelation),
              mean((interest$highFiLowCorrelation)^2),
              mean(interest$relativeError),
              colMeans(calculate_local_probabilites(interest$localCorrelation, localCorrs)))
    
    
    # for(corr in localCorrs){
    #   vals <- c(vals, mean(calculate_local_probability_absolute(interest$localCorrelation, corr)))
    # }
    # vals <- c(vals, mean(calculate_local_probability_coefficient_of_variation(interest$localCorrelation)))
    # vals <- c(vals, mean(calculate_local_probability_variation(interest$localCorrelation)))
    #vals <- c(vals, 0)
    processedData[entries, ] <- vals
  }
  cat(" - done\n")
  colNames <- c("instance",
                "globalCorrelation",
                "globalCorrelation_R2",
                "lowFiLowCorrelation",
                "lowFiLowCorrelation_R2",
                "highFiLowCorrelation",
                "highFiLowCorrelation_R2",
                "relativeError",
                "localCorrelationSD",
                "localCorrelationCoefficientOfVariation",
                "localCorrelationSD_R2",
                "localCorrelationCoefficientOfVariation_R2",
                rep("", 4*length(localCorrs)))
  
  for(k in 1:length(localCorrs)){
    colNames[[12 + k + 0*length(localCorrs)]] <- paste0("localCorrelationAbs_corr", localCorrs[[k]])
    colNames[[12 + k + 1*length(localCorrs)]] <- paste0("localCorrelationR2_corr", localCorrs[[k]])
    colNames[[12 + k + 2*length(localCorrs)]] <- paste0("localCorrelation_corr", localCorrs[[k]])
    colNames[[12 + k + 3*length(localCorrs)]] <- paste0("localCorrelationNeg_corr", localCorrs[[k]])
  }
  # 
  # for(corr in localCorrs){
  #   colNames <- c(colNames, paste0("localCorrelationAbs_corr", corr))
  # }
  # colNames <- c(colNames, "localCorrelationCoefficientOfVariation", "localCorrelationVariance")
  colnames(processedData) <- colNames
  return(processedData)
}


dataWithAlgorithmPerformances <- function(combinedData, featuresData){
  
  # First thing is to define algorithms based on the data
  algorithms <- c()
  for(technique in unique(combinedData$technique)){
    for(initialBudgetHigh in unique(combinedData[combinedData$technique == technique, "initialBudgetHigh"])){
      for(initialBudgetLow in unique(combinedData[combinedData$technique == technique & combinedData$initialBudgetHigh == initialBudgetHigh, "initialBudgetLow"])){
        algorithms <- c(algorithms, paste0(technique, "_", initialBudgetHigh, "_", initialBudgetLow))
      }
    }
  }
  instances <- c()
  for(instance in unique(featuresData$instance)){
    for(sampleStrategy in unique(combinedData[combinedData$instance == instance, "sampleStrategy"])){
      for(budget in unique(combinedData[combinedData$instance == instance & combinedData$sampleStrategy == sampleStrategy, "budget"])){
        for(costRatio in unique(combinedData[combinedData$instance == instance & combinedData$sampleStrategy == sampleStrategy & combinedData$budget == budget, "costRatio"])){
          instances <- c(instances, paste0(sampleStrategy, "_", budget, "_", costRatio, "_", instance))
        }
      }
    }
  }
  processedData <- data.frame(matrix(ncol = (ncol(featuresData) + 3 + 5 * length(algorithms) + 5), nrow = 0))
  entries <- 0
  for(instanceName in instances){
    #if(instanceName == "modelAccuracy_50_0.1_WangRastrigin-d10-error3-phi10000."){next}
    #if(instanceName == "modelAccuracy_5_0.1_COCOfunction17-dim1-seed1-globalNoise10-centres9-radius0.1-noise6-amp0.1"){next}
    entries <- entries + 1
    #print(paste0(entries, ": ", instanceName))
    # Extract instance info
    split <- strsplit(instanceName, split = "_")
    sampleStrategy <- split[[1]][1]
    budget <- split[[1]][2]
    costRatio <- split[[1]][3]
    instance <- split[[1]][4]
    # First add features
    vals <- c(instanceName, as.numeric(budget)/5, budget, costRatio, featuresData[featuresData$instance == instance, -1])
    # Grab desired data rows
    interest <- combinedData[combinedData$instance == instance &
                               combinedData$sampleStrategy == sampleStrategy &
                               combinedData$budget == budget &
                               combinedData$costRatio == costRatio, ]
    # Make sure each algorithm at least has one entry, if not skip this
    allFound <- TRUE
    for(algorithmName in algorithms){
      split <- strsplit(algorithmName, split = "_")
      technique <- split[[1]][1]
      initialBudgetHigh <- split[[1]][2]
      initialBudgetLow <- split[[1]][3]
      if(nrow(interest[interest$technique == technique &
                 interest$initialBudgetHigh == initialBudgetHigh &
                 interest$initialBudgetLow == initialBudgetLow, ]) == 0){
        print(paste0("Problem with ", algorithmName))
        allFound <- FALSE
        break
      }
    }
    if(!allFound){
      print(paste0("Skipping instance ", instance, " as of the algorithms did not complete a single run"))
      next
    }
    # Now deal with each of the algorithms, start with means, also do medians
    means <- c()
    medians <- c()
    for(algorithmName in algorithms){
      # Extract data
      split <- strsplit(algorithmName, split = "_")
      technique <- split[[1]][1]
      initialBudgetHigh <- split[[1]][2]
      initialBudgetLow <- split[[1]][3]
      if(is.nan(mean(interest[interest$technique == technique &
                       interest$initialBudgetHigh == initialBudgetHigh &
                       interest$initialBudgetLow == initialBudgetLow, "performance"]))){
        print("Got NAN!")
        print(interest[interest$technique == technique &
                         interest$initialBudgetHigh == initialBudgetHigh &
                         interest$initialBudgetLow == initialBudgetLow, "performance"])
        
      }
      means <- c(means, mean(interest[interest$technique == technique &
                                        interest$initialBudgetHigh == initialBudgetHigh &
                                        interest$initialBudgetLow == initialBudgetLow, "performance"]))
      medians <- c(medians, median(interest[interest$technique == technique &
                                        interest$initialBudgetHigh == initialBudgetHigh &
                                        interest$initialBudgetLow == initialBudgetLow, "performance"]))
      
    }
    bestMeans <- ""
    for(i in 1:length(algorithms)){
      vals <- c(vals, means[[i]], min(means) * 1.05 > means[[i]])
      if(min(means) * 1.05 > means[[i]] & bestMeans == ""){bestMeans <- algorithms[[i]]}
      else if(min(means) * 1.05 > means[[i]] & bestMeans != ""){bestMeans <- "None"}
    }
    vals <- c(vals, min(means))
    vals <- c(vals, bestMeans)
    
    
    bestMedians <- ""
    for(i in 1:length(algorithms)){
      vals <- c(vals, medians[[i]], min(medians) * 1.05 > medians[[i]])
      if(min(medians) * 1.05 > medians[[i]] & bestMedians == ""){bestMedians <- algorithms[[i]]}
      else if(min(medians) * 1.05 > medians[[i]] & bestMedians != ""){bestMedians <- "None"}
    }
    vals <- c(vals, min(medians))
    vals <- c(vals, bestMedians)
    
    
    
    # Finally deal with Wilcoxon comparison
    bestWilcoxon <- ""
    
    for(algorithmNameOne in algorithms){
      # Extract data
      split <- strsplit(algorithmNameOne, split = "_")
      techniqueOne <- split[[1]][1]
      initialBudgetHighOne <- split[[1]][2]
      initialBudgetLowOne <- split[[1]][3]
      
      firstResults <- interest[interest$technique == techniqueOne &
                                 interest$initialBudgetHigh == initialBudgetHighOne &
                                 interest$initialBudgetLow == initialBudgetLowOne, "performance"]
      
      better <- TRUE
      
      
      for(algorithmNameTwo in algorithms){
        if(algorithmNameOne == algorithmNameTwo){next}
        saveFirstResults <- firstResults
        # Extract data
        split <- strsplit(algorithmNameTwo, split = "_")
        techniqueTwo <- split[[1]][1]
        initialBudgetHighTwo <- split[[1]][2]
        initialBudgetLowTwo <- split[[1]][3]
        
        secondResults <- interest[interest$technique == techniqueTwo &
                                   interest$initialBudgetHigh == initialBudgetHighTwo &
                                   interest$initialBudgetLow == initialBudgetLowTwo, "performance"]
        
        # Need to check have the same number of entries, should have but double check and if not print a warning
        if(length(secondResults) != length(firstResults)){
          print("Problem! Have an instance with different number of entries!")
          print(paste0("Instance is ", instanceName))
          print(paste0("Algorithms are ", techniqueOne, "-", initialBudgetHighOne, "-", initialBudgetLowOne, "(", length(firstResults), ") and ",
                       techniqueTwo, "-", initialBudgetHighTwo, "-", initialBudgetLowTwo, "(", length(secondResults), ")"))
          firstResults <- firstResults[1:min(length(secondResults), length(firstResults))]
          secondResults <- secondResults[1:min(length(secondResults), length(firstResults))]
        }
        
        
        # First test the hypothesis that the two have the same median distribution
        hypSame <- wilcox.test(secondResults, firstResults)
        #if(hypSame$p.value > 0.05){next}
        # Test hypothesis second has a lower mean
        #hypFirstSameOrLower <- wilcox.test(secondResults, firstResults, alternative = "l")
        hypFirstSameOrLower <- wilcox.test(secondResults, firstResults, alternative = "l")
        
        #better <- better & (hypSame$p.value > 0.05 | hypFirstSameOrLower$p.value > 0.05)
        #better <- better & (hypFirstSameOrLower$p.value > 0.05)
        better <- better & (hypSame$p.value > 0.05 | median(firstResults) < median(secondResults))
        
        firstResults <- saveFirstResults
      }
      vals <- c(vals, better)
      if(better & bestWilcoxon == ""){bestWilcoxon <- algorithmNameOne}
      else if(better & bestWilcoxon != ""){bestWilcoxon <- "None"}
    }
    vals <- c(vals, bestWilcoxon)
    processedData[entries, ] <- vals
    
  }
  
  colNames <- c("instance", "dimension", "budget", "costRatio", colnames(featuresData[, -1]))
  for(i in 1:length(algorithms)){
    colNames <- c(colNames, paste0(algorithms[[i]], "-mean"), paste0(algorithms[[i]], "-goodMean"))
  }
  colNames <- c(colNames, "bestMean", "bestPerformanceMean")
  for(i in 1:length(algorithms)){
    colNames <- c(colNames, paste0(algorithms[[i]], "-median"), paste0(algorithms[[i]], "-goodMedian"))
  }
  colNames <- c(colNames, "bestMedian", "bestPerformanceMedian")
  
  for(i in 1:length(algorithms)){
    colNames <- c(colNames, paste0(algorithms[[i]], "-goodWilcoxon"))
  }
  colNames <- c(colNames, "bestPerformanceWilcoxon")
  colnames(processedData) <- colNames
  
  return(processedData)
}


combineArrayResults <- function(runName, arrayStart, arrayEnd, jobsPerArray = 1){
  options(scipen=999)
  if(jobsPerArray == 1){combinedData <- read.table(paste0("../data/clusterResults/", runName, "_arrayJob", arrayStart, ".txt"), header = TRUE, sep = " ", fill = TRUE)}
  else{combinedData <- read.table(paste0("../data/clusterResults/", runName, "_arrayJob", (arrayStart - 1) * jobsPerArray + 1, "-", arrayStart * jobsPerArray, ".txt"), header = TRUE, sep = " ", fill = TRUE)}
  if(arrayEnd == arrayStart){return(combinedData)}
  for(i in (arrayStart + 1):arrayEnd){
    cat(paste0("\rCombining files, done ", i, "/", arrayEnd))
    if(jobsPerArray == 1){filename <- paste0("../data/clusterResults/", runName, "_arrayJob", i, ".txt")}
    else{filename <- paste0("../data/clusterResults/", runName, "_arrayJob", (i - 1) * jobsPerArray + 1, "-", i * jobsPerArray, ".txt")}
    
    if(!file.exists(filename)){
      print(paste0("Skipping file ", filename, " as it does not exist!"))
      next
    }
    newData <- read.table(filename, header = TRUE, sep = " ", fill = TRUE)
    combinedData <- rbind(combinedData, newData)
  }
  cat(" - done!\n")
  options(scipen=0)
  return(combinedData)
}


combineAndAverageResults <- function(runName, arrayStart, arrayEnd, jobsPerArray = 1, localCorrs = c(0.85, 0.9, 0.95, 0.975)){
  combinedData <- combineArrayResults(runName, arrayStart, arrayEnd, jobsPerArray)
  return(dataWithSimpleFeatures(combinedData, localCorrs))
}


combineAndProcessResults <- function(runName, arrayStart, arrayEnd, jobsPerArray = 1, localCorrs = c(0.85, 0.9, 0.95, 0.975)){
  combinedData <- combineArrayResults(runName, arrayStart, arrayEnd, jobsPerArray)
  #combinedData <- rbind(combinedData[combinedData$technique == "cokriging", ], combinedData[combinedData$technique == "kriging", ])
  averagedData <- dataWithSimpleFeatures(combinedData, localCorrs)
  return(dataWithAlgorithmPerformances(combinedData, averagedData))
}

combineAndSaveResults <- function(runName, arrayStart, arrayEnd, jobsPerArray = 1){
  combinedData <- combineArrayResults(runName, arrayStart, arrayEnd, jobsPerArray)
  write.table(combinedData, paste0("../data/clusterResults/combinedResults/", runName, ".txt"), sep = " ", quote = FALSE, row.names = FALSE)
}

readAndProcessResults <- function(runName, localCorrs = c(0.85, 0.9, 0.95, 0.975)){
  combinedData <- read.table(paste0("../data/clusterResults/combinedResults/", runName, ".txt"), header = TRUE, sep = " ", fill = TRUE)
  averagedData <- dataWithSimpleFeatures(combinedData, localCorrs)
  return(dataWithAlgorithmPerformances(combinedData, averagedData))
}

combineAndProcessResultsWithoutKriging <- function(runName, arrayStart, arrayEnd, jobsPerArray = 1, localCorrs = c(0.85, 0.9, 0.95, 0.975)){
  combinedData <- combineArrayResults(runName, arrayStart, arrayEnd, jobsPerArray)
  combinedData <- combinedData[combinedData$technique == "cokriging", ]
  averagedData <- dataWithSimpleFeatures(combinedData, localCorrs)
  return(dataWithAlgorithmPerformances(combinedData, averagedData))
}

combineAndProcessResultsWithoutCoKriging <- function(runName, arrayStart, arrayEnd, jobsPerArray = 1, localCorrs = c(0.85, 0.9, 0.95, 0.975)){
  combinedData <- combineArrayResults(runName, arrayStart, arrayEnd, jobsPerArray)
  combinedData <- combinedData[combinedData$technique == "kriging", ]
  averagedData <- dataWithSimpleFeatures(combinedData, localCorrs)
  return(dataWithAlgorithmPerformances(combinedData, averagedData))
}

#data <- combineAndProcessResults("COCOinterestingFunctionsd1d5", 1, 964)

#data <- combineAndProcessResults("COCOinterestingFunctionsd1d5", 1, 964)


#data <- combineArrayResults("COCOinterestingFunctionsd1d5", 1, 964)
#featureData <- dataWithSimpleFeatures(data, c(0.85, 0.9, 0.95, 0.975))
#fullData <- dataWithAlgorithmPerformances(data, featureData)
#interest <- data[data$instance == "COCOfunction17-dim1-seed1-globalNoise10-centres9-radius0.1-noise6-amp0.1", ]

