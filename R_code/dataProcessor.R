source("R_code/libraries.R")
# Functions which take the experimental results and process them so that they can be plotted
# and analysed

# Adding a cost ratio of 0.1 as this is fixed throughout, naming it so it is easy to
# modify when further work is done 
costRatio <- 0.1
# Basic functions which combines all of the experimental results given into a single
# dataframe. No processing is performed, only a storing of the results.
combineArrayResults <- function(runName, arrayStart, arrayEnd, jobsPerArray = 1, printInfo = TRUE){
  options(scipen=999)
  if(jobsPerArray == 1){combinedData <- read.table(paste0("data/clusterResults/", runName, "_arrayJob", arrayStart, ".txt"), header = TRUE, sep = " ", fill = TRUE)}
  else{combinedData <- read.table(paste0("data/clusterResults/", runName, "_arrayJob", (arrayStart - 1) * jobsPerArray + 1, "-", arrayStart * jobsPerArray, ".txt"), header = TRUE, sep = " ", fill = TRUE)}
  if(arrayEnd == arrayStart){return(combinedData)}
  for(i in (arrayStart + 1):arrayEnd){
    if(printInfo){cat(paste0("\rCombining files, done ", i, "/", arrayEnd))}
    if(jobsPerArray == 1){filename <- paste0("data/clusterResults/", runName, "_arrayJob", i, ".txt")}
    else{filename <- paste0("data/clusterResults/", runName, "_arrayJob", (i - 1) * jobsPerArray + 1, "-", i * jobsPerArray, ".txt")}
    
    if(!file.exists(filename)){
      print(paste0("Skipping file ", filename, " as it does not exist!"))
      next
    }
    newData <- read.table(filename, header = TRUE, sep = " ", fill = TRUE)
    combinedData <- rbind(combinedData, newData)
  }
  if(printInfo){cat(" - done.\n")}
  options(scipen=0)
  return(combinedData)
}


# Combines experimental results and takes average of instance features, ignoring
# model performance.
dataWithSimpleFeatures <- function(data, corrs, printInfo = TRUE){
  featureNames <- c("dimension", "budget", "CC", "RRMSE", "LCC_sd", "LCC_coeff")
  for(corr in corrs){
    featureNames <- c(featureNames, paste0("LCC_", corr))
  }
  colNum <- 1 + length(featureNames)
  processedData <- data.frame(matrix(ncol = colNum, nrow = 0))
  entries <- 0
  # Add budget column
  data$budget <- data$highFiBudget + costRatio * data$lowFiBudget
  for(instance in unique(data$instance)){
    interest <- data[data$instance == instance, ]
    for(budget in unique(interest$budget)){
      interest <- interest[interest$budget == budget, ]
      entries <- entries + 1
      if(printInfo){cat("\rWorking on features, row ", entries)}
      # Save averages!
      processedData[entries, ] <- c(instance, colMeans(interest[, featureNames]))
    }
  }
  if(printInfo){cat(" - done.\n")}
  colnames(processedData) <- c("instance", featureNames)
  return(processedData)
}


# Creates data for which each instance / budget pair has a row with its
# features, as well as the performances of Kriging and CoKriging and the
# best performing model according to the Wilcoxon test.
dataWithAlgorithmPerformances <- function(combinedData, featuresData, printInfo = TRUE){
  processedData <- data.frame(matrix(ncol = (ncol(featuresData) + 5), nrow = 0))
  entries <- 0
  # Add budget column
  combinedData$budget <- combinedData$highFiBudget + costRatio * combinedData$lowFiBudget
  for(instance in unique(featuresData$instance)){
    interest <- combinedData[combinedData$instance == instance, ]
    for(budget in unique(interest$budget)){
      interest <- interest[interest$budget == budget, ]
      entries <- entries + 1
      if(printInfo){cat("\rWorking on algorithm performance, row ", entries)}
      # First add features
      interestFeatures <- featuresData[featuresData$instance == instance & 
                                         featuresData$budget == budget, ]
 
      vals <- c(interestFeatures[1, ])
      # Grab desired Kriging and CoKriging data rows
      interestKriging <- interest[interest$technique == "kriging", ]
      interestCokriging <- interest[interest$technique == "cokriging", ]
      if(nrow(interestCokriging) == 0 || nrow(interestKriging) == 0){
        print(paste0("Do not have entries for both Kriging and CoKriging for instance ", instance, " with budget ", budget, ", skipping this one"))
              entries <- entries - 1
              next
      }
      # Get medians
      krigMedian <- median(interestKriging$performance)
      cokrigMedian <- median(interestCokriging$performance)
      # Add a check that we have enough values of each
      if(nrow(interestKriging) != nrow(interestCokriging)){
        print(paste0("Do not have the same number of values for kriging and cokriging for instance ", instance, " with budget ", budget, "! Taking the minimum."))
        minSize <- min(nrow(interestKriging), nrow(interestCokriging))
        interestKriging <- interestKriging[1:minSize, ]
        interestCokriging <- interestCokriging[1:minSize, ]
      }
      # Compare using Wilcoxon test
      # First test the hypothesis that the two have the same median distribution
      hypSame <- wilcox.test(interestKriging$performance, interestCokriging$performance)
      if(hypSame$p.value > 0.05){
        krigGood <- TRUE
        cokrigGood <- TRUE
        superiorModel <- "Tied"
      }else{
        if(krigMedian > cokrigMedian){
          krigGood <- FALSE
          cokrigGood <- TRUE
          superiorModel <- "Co-Kriging"
        }else{
          krigGood <- TRUE
          cokrigGood <- FALSE
          superiorModel <- "Kriging"
        }
      }
      processedData[entries, ] <- c(vals, krigMedian, krigGood, cokrigMedian, cokrigGood, superiorModel)
    }
  }
  if(printInfo){cat(" - done.\n")}
  colnames(processedData) <- c(colnames(featuresData), "krigingMedian", "krigingGood", "cokrigingMedian", "cokrigingGood", "superiorModel")
  
  return(processedData)
}




# Combines experimental results and takes the averages of the features
# for every instance
combineAndAverageFeatures <- function(runName, arrayStart, arrayEnd, jobsPerArray = 1, localCorrs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.975), printInfo = TRUE){
  combinedData <- combineArrayResults(runName, arrayStart, arrayEnd, jobsPerArray, printInfo)
  return(dataWithSimpleFeatures(combinedData, localCorrs, printInfo))
}

# Combines experimental results and performs analysis on model performance,
# as well as averaging the measured features
combineAndProcessResults <- function(runName, arrayStart, arrayEnd, jobsPerArray = 1, localCorrs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.975), printInfo = TRUE){
  combinedData <- combineArrayResults(runName, arrayStart, arrayEnd, jobsPerArray)
  averagedData <- dataWithSimpleFeatures(combinedData, localCorrs)
  return(dataWithAlgorithmPerformances(combinedData, averagedData, printInfo))
}
