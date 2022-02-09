source("dataProcessor.R")
source("featureAnalysis.R")
source("modelPerformanceAnalysis.R")
library(plotly)
library(caret)
library(e1071)

graphScale <- 0.7
# This is the reference data when plotting performance, pretty sure it is the same for all. It is not, so start with surrogate model fitting
refData <- data.frame("algName" = c("cokriging_0.8_0.2", "None", "kriging_1_0"),
                      "label" = c("Co-Kriging", "Tied", "Kriging"),
                      "pch" = c(17, 16, 15),
                      "col" = c("green4", "darkgoldenrod1", "red"))
refData$col <- as.character(refData$col)
refData$label <- as.character((refData$label))
refData$algName <- as.character(refData$algName)
corrs <- c(seq(0.1, 0.9, 0.1), 0.95, 0.975)
# This is the creation of the graphs I have so far
# First read in data of current literature instances, and plot features and performance
dataset01_litSuiteSurrogateModelFit <- combineAndProcessResults("01-litSuiteSurrogateModelFit", 1, 906, localCorrs = corrs)
# Extra lines to add some legend to the feature plot
dataset01_litSuiteSurrogateModelFit$pch = 16
dataset01_litSuiteSurrogateModelFit$legend = "Fixed"
dataset01_litSuiteSurrogateModelFit$col = "black"
dataset01_litSuiteSurrogateModelFit[str_which(dataset01_litSuiteSurrogateModelFit$instance, "Wang", negate = FALSE), "pch"] = 17
dataset01_litSuiteSurrogateModelFit[str_which(dataset01_litSuiteSurrogateModelFit$instance, "Wang", negate = FALSE), "legend"] = "Error-based"
dataset01_litSuiteSurrogateModelFit[str_which(dataset01_litSuiteSurrogateModelFit$instance, "Wang", negate = FALSE), "col"] = "sienna3"
dataset01_litSuiteSurrogateModelFit[str_which(dataset01_litSuiteSurrogateModelFit$instance, "Toal", negate = FALSE), "pch"] = 15
dataset01_litSuiteSurrogateModelFit[str_which(dataset01_litSuiteSurrogateModelFit$instance, "Toal", negate = FALSE), "legend"] = "Parameter-based"
dataset01_litSuiteSurrogateModelFit[str_which(dataset01_litSuiteSurrogateModelFit$instance, "Toal", negate = FALSE), "col"] = "sienna4"
plotInstanceFeatures(dataset01_litSuiteSurrogateModelFit, "01-litSuiteSurrogateModelFit", corrs, mult = graphScale)
assessModelPerformance(dataset01_litSuiteSurrogateModelFit, "01-litSuiteSurrogateModelFit", corrs, refData, mult = graphScale)



dataset02_finalModelAccuracySuiteSurrogateModelFit <- combineAndProcessResults("00-finalModelAccuracySuiteSurrogateModelFit", 1, 1143, 5, localCorrs = corrs)
dataset02_finalModelAccuracySuiteSurrogateModelFit$pch = 4
dataset02_finalModelAccuracySuiteSurrogateModelFit$legend = "Disturbance-based"
dataset02_finalModelAccuracySuiteSurrogateModelFit$col = "sienna1"
# plotInstanceFeatures(dataset02_finalModelAccuracySuiteSurrogateModelFit, "02-newSuiteSurrogateModelFit", corrs, mult = graphScale)
# assessModelPerformance(dataset02_finalModelAccuracySuiteSurrogateModelFit, "02-newSuiteSuiteSurrogateModelFit", corrs, refData, mult = graphScale)

plotInstanceFeatures(rbind(dataset02_finalModelAccuracySuiteSurrogateModelFit, dataset01_litSuiteSurrogateModelFit), "03-enhancedSuiteSurrogateModelFit", corrs, mult = graphScale)
assessModelPerformance(rbind(dataset02_finalModelAccuracySuiteSurrogateModelFit, dataset01_litSuiteSurrogateModelFit), "03-enhancedSuiteSurrogateModelFit", corrs, refData, mult = graphScale)












# FORMAT SUBMITTED PAPER DATA SO IT MAKES SENSE TO SOMEONE NEW
corrs <- c(seq(0.1, 0.9, 0.1), 0.95, 0.975)
data <- combineArrayResults("01-litSuiteSurrogateModelFit", 1, 906)
#data$performance <- data$performance / (data$fMax - data$fMin)
data <- data[, c("instance", "technique", "seed", "dimension", "budget", "globalCorrelation", "relativeError", "localCorrelation", "performance")]
colnames(data) <- c("instance", "technique", "seed", "dimension", "budget", "CC", "RRMSE", "localCorrelation", "performance")
data$CC <- data$CC^2
localProbs <- calculate_local_probabilites(data$localCorrelation, corrs)
data$LCC_sd <- localProbs$localCorrelationSD_R2
data$LCC_coeff <- localProbs$localCorrelationCoeffVariation_R2
for(corr in corrs){
  data[, paste0("LCC_", corr)] <- localProbs[, paste0("localCorrR2_", corr)]
}
data <- subset(data, select = -c(localCorrelation))
data <- select(data, -performance, performance)
dataCompletedLit <- data
# features <- c("dimension", "budget", "CC", "RRMSE", "LCC_sd", "LCC_coeff")
# for(corr in corrs){
#   features <- c(features, paste0("LCC_", corr))
# }
# features <- c(features, "performance")
# dataAverage = data.frame()
# dataAverage[1:length(unique(data$instance)), "instance"] <- unique(data$instance)
# for(instance in dataAverage$instance){
#   dataAverage[dataAverage$instance == instance, features] <- colMeans(data[data$instance == instance &
#                                                                     data$technique == "kriging", features])
# }

dataProcessed <- combineAndProcessResults("01-litSuiteSurrogateModelFit", 1, 906, localCorrs = corrs)
keepFeatures <- c("instance", "dimension", "budget", "globalCorrelation_R2", "relativeError", "localCorrelationSD_R2", "localCorrelationCoefficientOfVariation_R2")
nameFeatures <- c("instance", "dimension", "budget", "CC", "RRMSE", "LCC_sd", "LCC_coeff")
for(corr in corrs){
  keepFeatures <- c(keepFeatures, paste0("localCorrelationR2_corr", corr))
  nameFeatures <- c(nameFeatures, paste0("LCC_", corr))
}
keepFeatures <- c(keepFeatures, "kriging_1_0-goodWilcoxon", "cokriging_0.8_0.2-goodWilcoxon", "bestPerformanceWilcoxon")
nameFeatures <- c(nameFeatures, "kriging-goodWilcoxon", "cokriging-goodWilcoxon", "superiorAlgorithmWilcoxon")
dataProcessed <- dataProcessed[, keepFeatures]
colnames(dataProcessed) <- nameFeatures
for(i in 1:nrow(dataProcessed)){
  dataProcessed[i, "instance"] <- tail(strsplit(dataProcessed[i, "instance"], split = "_")[[1]], 1)
}
dataProcessedCompletedLit <- dataProcessed

# Save it all!
write.csv(dataCompletedLit, file = "../data/analysis/discussion/litRawRunsData.csv", quote = FALSE, row.names = FALSE)
write.csv(dataProcessedCompletedLit, file = "../data/analysis/discussion/litProcessedData.csv", quote = FALSE, row.names = FALSE)



data <- combineArrayResults("00-finalModelAccuracySuiteSurrogateModelFit", 1, 1143, 5)
#data$performance <- data$performance / (data$fMax - data$fMin)
data <- data[, c("instance", "technique", "seed", "dimension", "budget", "globalCorrelation", "relativeError", "localCorrelation", "performance")]
colnames(data) <- c("instance", "technique", "seed", "dimension", "budget", "CC", "RRMSE", "localCorrelation", "performance")
data$CC <- data$CC^2
localProbs <- calculate_local_probabilites(data$localCorrelation, corrs)
data$LCC_sd <- localProbs$localCorrelationSD_R2
data$LCC_coeff <- localProbs$localCorrelationCoeffVariation_R2
for(corr in corrs){
  data[, paste0("LCC_", corr)] <- localProbs[, paste0("localCorrR2_", corr)]
}
data <- subset(data, select = -c(localCorrelation))
data <- select(data, -performance, performance)
dataCompletedAug <- data
# features <- c("dimension", "budget", "CC", "RRMSE", "LCC_sd", "LCC_coeff")
# for(corr in corrs){
#   features <- c(features, paste0("LCC_", corr))
# }
# features <- c(features, "performance")
# dataAverage = data.frame()
# dataAverage[1:length(unique(data$instance)), "instance"] <- unique(data$instance)
# for(instance in dataAverage$instance){
#   dataAverage[dataAverage$instance == instance, features] <- colMeans(data[data$instance == instance &
#                                                                     data$technique == "kriging", features])
# }

dataProcessed <- combineAndProcessResults("00-finalModelAccuracySuiteSurrogateModelFit", 1, 1143, 5, localCorrs = corrs)
keepFeatures <- c("instance", "dimension", "budget", "globalCorrelation_R2", "relativeError", "localCorrelationSD_R2", "localCorrelationCoefficientOfVariation_R2")
nameFeatures <- c("instance", "dimension", "budget", "CC", "RRMSE", "LCC_sd", "LCC_coeff")
for(corr in corrs){
  keepFeatures <- c(keepFeatures, paste0("localCorrelationR2_corr", corr))
  nameFeatures <- c(nameFeatures, paste0("LCC_", corr))
}
keepFeatures <- c(keepFeatures, "kriging_1_0-goodWilcoxon", "cokriging_0.8_0.2-goodWilcoxon", "bestPerformanceWilcoxon")
nameFeatures <- c(nameFeatures, "kriging-goodWilcoxon", "cokriging-goodWilcoxon", "superiorAlgorithmWilcoxon")
dataProcessed <- dataProcessed[, keepFeatures]
colnames(dataProcessed) <- nameFeatures
for(i in 1:nrow(dataProcessed)){
  dataProcessed[i, "instance"] <- tail(strsplit(dataProcessed[i, "instance"], split = "_")[[1]], 1)
}
dataProcessedCompletedAug <- dataProcessed

# Save it all!
write.csv(rbind(dataCompletedLit, dataCompletedAug), file = "../data/analysis/discussion/augRawRunsData.csv", quote = FALSE, row.names = FALSE)
write.csv(rbind(dataProcessedCompletedLit, dataProcessedCompletedAug), file = "../data/analysis/discussion/augProcessedData.csv", quote = FALSE, row.names = FALSE)

# DONE WITH IT!!







# Extract all created instances and choose subset based on LCC feature
dataBigReduced <- combineAndAverageResults("COCOinstanceFeaturesNew", 1, 1, 86, localCorrs = corrs)
for(i in 2:1005){
  print(i)
  dataBigReduced <- rbind(dataBigReduced, combineAndAverageResults("COCOinstanceFeaturesNew", i, i, 86, localCorrs = corrs))
}
# Remove functions for which d = 1 is not defined
for(func in c(8, 9, 17, 18, 19)){
  dataBigReduced <- dataBigReduced[str_which(dataBigReduced$instance, paste0("function", func, "-dim1-"), negate = TRUE),]
}
# Save data
write.table(dataBigReduced, "../data/clusterResults/combinedResults/COCObigDataNew.txt", quote = FALSE, row.names = FALSE, col.names = TRUE, sep = " ")
dataBigReducedCheck <- read.table("../data/clusterResults/combinedResults/COCObigDataNew.txt", header = TRUE, sep = " ")

# Plot it
dataBigReduced$pch = 4
dataBigReduced$legend = "Disturbance-based"
dataBigReduced$col = "sienna1"
plotInstanceFeatures(dataBigReduced, "COCObigDataNew", corrs, mult = graphScale, legend = FALSE)
# Work on finding a subset
started <- FALSE
count <- 0
for(corrTop in seq(1, 0.05, -0.05)){
  if(corrTop == 1){tempData <- dataBigReduced[dataBigReduced$globalCorrelation_R2 <= corrTop & dataBigReduced$globalCorrelation_R2 >= corrTop - 0.05, ]}
  else{tempData <- dataBigReduced[dataBigReduced$globalCorrelation_R2 < corrTop & dataBigReduced$globalCorrelation_R2 >= corrTop - 0.05, ]}
  for(prob in seq(0, 0.95, 0.05)){
    if(prob == 0.95){tempData2 <- tempData[tempData$localCorrelationR2_corr0.5 >= prob & tempData$localCorrelationR2_corr0.5 <= prob + 0.05, ]}
    else{tempData2 <- tempData[tempData$localCorrelationR2_corr0.5 >= prob & tempData$localCorrelationR2_corr0.5 < prob + 0.05, ]}
    tempData3 <- tempData2
    for(dim in c(1, 2, 5, 10)){
      count <- count + 1
      cat(paste0(count, " - Working on corr ", corrTop, " prob ", prob, " and dim ", dim))
      tempData4 <- tempData3[str_which(tempData3$instance, paste0("-dim", dim, "-"), negate = FALSE), ]
      # Grab a random one if there are multiple
      if(nrow(tempData4) == 0){
        cat(" - skipped\n")
        next
      }
      cat("\n")
      tempData4 <- tempData4[sample(1:nrow(tempData4), 1), ]
      if(!started){
        subsetData <- tempData4
        started <- TRUE
      }
      else{subsetData <- rbind(subsetData, tempData4)}
    }
  }
}
plotInstanceFeatures(subsetData, "COCOsubsetDataLocalCorr0-5", corrs, mult = graphScale)
COCOsubsetDataLocalCorr0.5 <- subsetData
write.table(COCOsubsetDataLocalCorr0.5$instance, paste0("../data/availableFunctions/COCOsubsetDataLocalCorr0-5.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)






















#dataset02_modelAccuracySuiteSurrogateModelFit <- combineAndProcessResults("04-modelAccuracySuiteSurrogateModelFit", 1, 1098, 6, localCorrs = corrs)
#dataset02_modelAccuracySuiteSurrogateModelFit$pch = 4
#dataset02_modelAccuracySuiteSurrogateModelFit$legend = "Disturbance-based"
#plotInstanceFeatures(dataset02_modelAccuracySuiteSurrogateModelFit, "02-modelAccuracySuiteSurrogateModelFit", corrs, mult = graphScale)
#assessModelPerformance(dataset02_modelAccuracySuiteSurrogateModelFit, "02-modelAccuracySuiteSurrogateModelFit", corrs, refData, mult = graphScale)

#plotInstanceFeatures(rbind(dataset01_litSuiteSurrogateModelFit, dataset02_modelAccuracySuiteSurrogateModelFit), "03-enhancedSuiteSurrogateModelFit", corrs, mult = graphScale)
#assessModelPerformance(rbind(dataset01_litSuiteSurrogateModelFit, dataset02_modelAccuracySuiteSurrogateModelFit), "03-enhancedSuiteSurrogateModelFit", corrs, refData, mult = graphScale)



#dataset07_functionOptSuiteSurrogateModelFit <- combineAndProcessResults("07-functionOptSuiteSurrogateModelFit", 1, 1083, 3)
#plotInstanceFeatures(dataset07_functionOptSuiteSurrogateModelFit, "04-functionOptSuiteSurrogateModelFit", c(0.85, 0.9, 0.95, 0.975), mult = graphScale)
#assessModelPerformance(dataset07_functionOptSuiteSurrogateModelFit, "04-functionOptSuiteSurrogateModelFit", c(0.85, 0.9, 0.95, 0.975), refData, mult = graphScale)


# 
# # Next work with surrogate model exploration
# refData <- data.frame("algName" = c("None", "cokriging_0.4_0.1", "kriging_0.5_0"),
#                       "label" = c("None", "CoKriging", "Kriging"),
#                       "pch" = c(16, 16, 16),
#                       "col" = c("green", "blue", "red"))
# refData$col <- as.character(refData$col)
# dataset02a_litSuiteSurrogateModelExploration <- combineAndProcessResults("02a-litSuiteSurrogateModelExploration", 1, 686)
# #dataset02b_litSuiteSurrogateModelExploration <- combineAndProcessResults("02b-litSuiteSurrogateModelExploration", 1, 2360)
# assessModelPerformance(dataset02a_litSuiteSurrogateModelExploration, "02-litSuiteSurrogateModelExploration", c(0.85, 0.9, 0.95, 0.975), refData, mult = graphScale)
# 
# dataset05a_modelAccuracySuiteSurrogateModelExploration <- combineAndProcessResults("05a-modelAccuracySuiteSurrogateModelExploration", 1, 4208)
# #dataset05b_modelAccuracySuiteSurrogateModelExploration <- combineAndProcessResults("05b-modelAccuracySuiteSurrogateModelExploration", 1, 15600)
# assessModelPerformance(dataset05a_modelAccuracySuiteSurrogateModelExploration, "05-modelAccuracySuiteSurrogateModelExploration", c(0.85, 0.9, 0.95, 0.975), refData, mult = graphScale)
# 
# #dataset08a_functionOptSuiteSurrogateModelExploration <- combineAndProcessResults("08a-functionOptSuiteSurrogateModelExploration", 1, 2102)
# #dataset08b_functionOptSuiteSurrogateModelExploration <- combineAndProcessResults("08b-functionOptSuiteSurrogateModelExploration", 1, 7400)
# #assessModelPerformance(dataset08a_functionOptSuiteSurrogateModelExploration, "08-functionOptSuiteSurrogateModelExploration", c(0.85, 0.9, 0.95, 0.975), refData, mult = graphScale)
# 
# 
# # Finally surrogate model exploitation
# dataset03a_litSuiteSurrogateModelExploitation <- combineAndProcessResults("03a-litSuiteSurrogateModelExploitation", 1, 686)
# #dataset03b_litSuiteSurrogateModelExploitation <- combineAndProcessResults("03b-litSuiteSurrogateModelExploitation", 1, 2360)
# assessModelPerformance(dataset03a_litSuiteSurrogateModelExploitation, "03-litSuiteSurrogateModelExploitation", c(0.85, 0.9, 0.95, 0.975), refData, mult = graphScale)
# 
# #dataset06a_modelAccuracySuiteSurrogateModelExploitation <- combineAndProcessResults("06a-modelAccuracySuiteSurrogateModelExploitation", 1, 4208)
# #dataset06b_modelAccuracySuiteSurrogateModelExploitation <- combineAndProcessResults("06b-modelAccuracySuiteSurrogateModelExploitation", 1, 15600)
# #assessModelPerformance(dataset06b_modelAccuracySuiteSurrogateModelExploitation, "06-modelAccuracySuiteSurrogateModelExploitation", c(0.85, 0.9, 0.95, 0.975), refData, mult = graphScale)
# 
# dataset09a_functionOptSuiteSurrogateModelExploitation <- combineAndProcessResults("09a-functionOptSuiteSurrogateModelExploitation", 1, 2102)
# #dataset09b_functionOptSuiteSurrogateModelExploitation <- combineAndProcessResults("09b-functionOptSuiteSurrogateModelExploitation", 1, 7400)
# # Remove next line eventually
# plotInstanceFeatures(dataset09a_functionOptSuiteSurrogateModelExploitation, "09-functionOptSuiteSurrogateModelExploitation", c(0.85, 0.9, 0.95, 0.975), mult = graphScale)
# assessModelPerformance(dataset09a_functionOptSuiteSurrogateModelExploitation, "09-functionOptSuiteSurrogateModelExploitation", c(0.85, 0.9, 0.95, 0.975), refData, mult = graphScale)
# 
# 
# 
# 
# dataset09a_functionOptSuiteSurrogateModelExploitation <- combineAndProcessResults("09a-functionOptSuiteSurrogateModelExploitation", 1, 2102)
# 




# CODE USED TO SELECT SUBSET
# # First big data
# #dataBig <- combineAndAverageResults("COCOinstanceFeatures", 1, 1000, 1008)
# corrs <- c(seq(0.05, 0.95, 0.1), 0.975)
# graphScale <- 0.7
# dataBig <- combineAndAverageResults("COCOinstanceFeatures", 1, 1, 1008, localCorrs = corrs)
# for(i in 2:1000){
#   print(i)
#   dataBig <- rbind(dataBig, combineAndAverageResults("COCOinstanceFeatures", i, i, 1008, localCorrs = corrs))
# }
# #write.table(dataBig, "../data/clusterResults/combinedResults/COCObigData.txt", quote = FALSE, row.names = FALSE, col.names = TRUE, sep = " ")
# dataBigCheck <- read.table("../data/clusterResults/combinedResults/COCObigData.txt", header = TRUE, sep = " ", fill = TRUE)
# dataBigCheck$pch = 4
# dataBigCheck$legend = "Disturbance-based"
# #plotInstanceFeatures(dataBigCheck, "COCObigData", c(0.85, 0.9, 0.95, 0.975))

# 
# # Next want to find subsets for big data
# # "Trim" big data by removing repeat and non sensical functions (i.e. functions 2, 4, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18, 19 with dimension 1)
# bigDataTrimmed <- dataBigCheck
# for(func in c(2, 4, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18, 19)){
#   bigDataTrimmed <- bigDataTrimmed[str_which(bigDataTrimmed$instance, paste0("function", func, "-dim1-"), negate = TRUE), ]
# }
# plotInstanceFeatures(bigDataTrimmed, "COCObigData", corrs, graphScale, legend = FALSE)
# 
# # interest <- bigDataTrimmed[bigDataTrimmed$globalCorrelation_R2 < 0.3 & bigDataTrimmed$localCorrelationR2_corr0.95 > 0.9, 
# #                            c("instance", "globalCorrelation_R2", "localCorrelationR2_corr0.95")]

# # USED FOR FINAL SUBSET
# started <- FALSE
# count <- 0
# for(corrTop in seq(1, 0.05, -0.05)){
#   if(corrTop == 1){tempData <- bigDataTrimmed[bigDataTrimmed$globalCorrelation_R2 <= corrTop & bigDataTrimmed$globalCorrelation_R2 >= corrTop - 0.05, ]}
#   else{tempData <- bigDataTrimmed[bigDataTrimmed$globalCorrelation_R2 < corrTop & bigDataTrimmed$globalCorrelation_R2 >= corrTop - 0.05, ]}
#   for(prob in seq(0, 0.95, 0.05)){
#     if(prob == 0.95){tempData2 <- tempData[tempData$localCorrelationR2_corr0.5 >= prob & tempData$localCorrelationR2_corr0.5 <= prob + 0.05, ]}
#     else{tempData2 <- tempData[tempData$localCorrelationR2_corr0.5 >= prob & tempData$localCorrelationR2_corr0.5 < prob + 0.05, ]}
#     tempData3 <- tempData2
#     for(dim in c(1, 2, 5, 10)){
#       count <- count + 1
#       cat(paste0(count, " - Working on corr ", corrTop, " prob ", prob, " and dim ", dim))
#       tempData4 <- tempData3[str_which(tempData3$instance, paste0("-dim", dim, "-"), negate = FALSE), ]
#       # Grab a random one if there are multiple
#       if(nrow(tempData4) == 0){
#         cat(" - skipped\n")
#         next
#       }
#       cat("\n")
#       tempData4 <- tempData4[sample(1:nrow(tempData4), 1), ]
#       if(!started){
#         subsetData <- tempData4
#         started <- TRUE
#       }
#       else{subsetData <- rbind(subsetData, tempData4)}
#     }
#   }
# }
# plotInstanceFeatures(subsetData, "COCOsubsetDataLocalCorr0-5", corrs, mult = graphScale)
# COCOsubsetDataLocalCorr0.5 <- subsetData
# write.table(COCOsubsetDataLocalCorr0.5$instance, paste0("../data/availableFunctions/COCOsubsetDataLocalCorr0-5.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)

# # Next two were used for surrogate model fit, surrogate model exploration and surrogate model exploitation
# started <- FALSE
# count <- 0
# for(corrTop in seq(1, 0.05, -0.05)){
#   if(corrTop == 1){tempData <- bigDataTrimmed[bigDataTrimmed$globalCorrelation_R2 <= corrTop & bigDataTrimmed$globalCorrelation_R2 >= corrTop - 0.05, ]}
#   else{tempData <- bigDataTrimmed[bigDataTrimmed$globalCorrelation_R2 < corrTop & bigDataTrimmed$globalCorrelation_R2 >= corrTop - 0.05, ]}
#   for(prob in seq(0, 0.95, 0.05)){
#     if(prob == 0.95){tempData2 <- tempData[tempData$localCorrelationR2_corr0.95 >= prob & tempData$localCorrelationR2_corr0.95 <= prob + 0.05, ]}
#     else{tempData2 <- tempData[tempData$localCorrelationR2_corr0.95 >= prob & tempData$localCorrelationR2_corr0.95 < prob + 0.05, ]}
#     tempData3 <- tempData2
#     for(dim in c(1, 2, 5, 10)){
#       count <- count + 1
#       cat(paste0(count, " - Working on corr ", corrTop, " prob ", prob, " and dim ", dim))
#       tempData4 <- tempData3[str_which(tempData3$instance, paste0("-dim", dim, "-"), negate = FALSE), ]
#       # Grab a random one if there are multiple
#       if(nrow(tempData4) == 0){
#         cat(" - skipped\n")
#         next
#       }
#       cat("\n")
#       tempData4 <- tempData4[sample(1:nrow(tempData4), 1), ]
#       if(!started){
#         subsetData <- tempData4
#         started <- TRUE
#       }
#       else{subsetData <- rbind(subsetData, tempData4)}
#     }
#   }
# }
# plotInstanceFeatures(subsetData, "COCOsubsetDataModelAccuracyR2_new", c(0.85, 0.9, 0.95, 0.975))
# subsetDataModelAccuracyR2 <- subsetData
# write.table(subsetData$instance, paste0("../data/availableFunctions/COCOsubsetDataModelAccuracyR2_new.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
# 
# # Now subset for optimisation purposes
# started <- FALSE
# count <- 0
# bigDataTrimmed$highFiLowCorrelation_R2 <- as.numeric(bigDataTrimmed$highFiLowCorrelation_R2)
# bigDataTrimmed$lowFiLowCorrelation_R2 <- as.numeric(bigDataTrimmed$lowFiLowCorrelation_R2)
# bigDataTrimmed$globalCorrelation_R2 <- as.numeric(bigDataTrimmed$globalCorrelation_R2)
# for(corrHighTop in seq(1, 0.05, -0.05)){
#   if(corrHighTop == 1){tempData <- bigDataTrimmed[bigDataTrimmed$highFiLowCorrelation_R2 >= corrHighTop - 0.05, ]}
#   else{tempData <- bigDataTrimmed[bigDataTrimmed$highFiLowCorrelation_R2 < corrHighTop & bigDataTrimmed$highFiLowCorrelation_R2 >= corrHighTop - 0.05, ]}
#   for(corrLowTop in seq(1, 0.05, -0.05)){
#     if(corrLowTop == 1){tempData2 <- tempData[tempData$globalCorrelation_R2 <= corrLowTop & tempData$globalCorrelation_R2 >= corrLowTop - 0.05, ]}
#     else{tempData2 <- tempData[tempData$globalCorrelation_R2 < corrLowTop & tempData$globalCorrelation_R2 >= corrLowTop - 0.05, ]}
#     #tempData2 <- tempData[tempData$lowFiLowCorrelation_R2 < corrLowTop & tempData$lowFiLowCorrelation_R2 >= corrLowTop - 0.05, ]
#     #tempData2 <- tempData[tempData$globalCorrelation_R2 < corrLowTop & tempData$globalCorrelation_R2 >= corrLowTop - 0.05, ]
#     tempData3 <- tempData2
#     for(dim in c(1, 2, 5, 10)){
#       count <- count + 1
#       #cat(paste0(count, " - Working on corrHigh ", corrHighTop, " corrLow ", corrLowTop, " corr ", corrTop, " and dim ", dim))
#       cat(paste0(count, " - Working on corrHigh ", corrHighTop, " globalCorr ", corrLowTop, " corr ", corrTop, " and dim ", dim))
#       tempData4 <- tempData3[str_which(tempData3$instance, paste0("-dim", dim, "-"), negate = FALSE), ]
#       if(nrow(tempData4) == 0){
#         cat(" - skipped\n")
#         next
#       }
#       cat("\n")
#       tempData4 <- tempData4[sample(1:nrow(tempData4), 1), ]
#       if(!started){
#         subsetData <- tempData4
#         started <- TRUE
#       }
#       else{subsetData <- rbind(subsetData, tempData4)}
#     }
#   }
# }
#   
# plotInstanceFeatures(subsetData, "COCOsubsetDataFunctionOptimisationR2", c(0.85, 0.9, 0.95, 0.975), 0.7)
# subsetDataFunctionOptimisationR2_new <- subsetData
# write.table(subsetData$instance, paste0("../data/availableFunctions/COCOsubsetDataFunctionOptimisationR2.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)


# started <- FALSE
# count <- 0
# for(corrTop in seq(1, -0.95, -0.05)){
#   tempData <- bigDataTrimmed[bigDataTrimmed$globalCorrelation < corrTop & bigDataTrimmed$globalCorrelation >= corrTop - 0.05, ]
#   for(prob in seq(0, 1, 0.05)){
#     tempData2 <- tempData[tempData$localCorrelation_corr0.975 >= prob & tempData$localCorrelation_corr0.975 < prob + 0.05, ]
#     for(probNeg in seq(0, 1, 0.05)){
#       tempData3 <- tempData2[tempData2$localCorrelationNeg_corr0.975 >= probNeg & tempData2$localCorrelationNeg_corr0.975 < probNeg + 0.05, ]
#       for(dim in c(1, 2, 5, 10)){
#         count <- count + 1
#         cat(paste0(count, " - Working on corr ", corrTop, " prob ", prob, " probNeg ", probNeg, " and dim ", dim))
#         tempData4 <- tempData3[str_which(tempData3$instance, paste0("-dim", dim, "-"), negate = FALSE), ]
#         # Grab a random one if there are multiple
#         if(nrow(tempData4) == 0){
#           cat(" - skipped\n")
#           next
#         }
#         cat("\n")
#         tempData4 <- tempData4[sample(1:nrow(tempData4), 1), ]
#         if(!started){
#           subsetData <- tempData4
#           started <- TRUE
#         }
#         else{subsetData <- rbind(subsetData, tempData4)}
#       }
#     }
#   }
# }
# plotInstanceFeatures(subsetData, "COCOsubsetDataModelAccuracy", c(0.85, 0.9, 0.95, 0.975))
# subsetDataModelAccuracy <- subsetData
# write.table(subsetData$instance, paste0("../data/availableFunctions/COCOsubsetDataModelAccuracy.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
# 
# # Now subset for optimisation purposes
# started <- FALSE
# count <- 0
# bigDataTrimmed$highFiLowCorrelation <- as.numeric(bigDataTrimmed$highFiLowCorrelation)
# bigDataTrimmed$lowFiLowCorrelation <- as.numeric(bigDataTrimmed$lowFiLowCorrelation)
# bigDataTrimmed$globalCorrelation <- as.numeric(bigDataTrimmed$globalCorrelation)
# for(corrHighTop in seq(1, -0.95, -0.05)){
#   tempData <- bigDataTrimmed[bigDataTrimmed$highFiLowCorrelation < corrHighTop & bigDataTrimmed$highFiLowCorrelation >= corrHighTop - 0.05, ]
#   for(corrLowTop in seq(1, -0.95, -0.05)){
#     tempData2 <- tempData[tempData$lowFiLowCorrelation < corrLowTop & tempData$lowFiLowCorrelation >= corrLowTop - 0.05, ]
#     for(corrTop in seq(1, -0.95, -0.05)){
#       #tempData3 <- tempData2[tempData2$globalCorrelation < corrTop & tempData2$globalCorrelation >= corrTop - 0.05, ]
#       if(corrTop != 1){next}
#       tempData3 <- tempData2
#       for(dim in c(1, 2, 5, 10)){
#         count <- count + 1
#         cat(paste0(count, " - Working on corrHigh ", corrHighTop, " corrLow ", corrLowTop, " corr ", corrTop, " and dim ", dim))
#         tempData4 <- tempData3[str_which(tempData3$instance, paste0("-dim", dim, "-"), negate = FALSE), ]
#         if(nrow(tempData4) == 0){
#           cat(" - skipped\n")
#           next
#         }
#         cat("\n")
#         tempData4 <- tempData4[sample(1:nrow(tempData4), 1), ]
#         if(!started){
#           subsetData <- tempData4
#           started <- TRUE
#         }
#         else{subsetData <- rbind(subsetData, tempData4)}
#       }
#     }
#   }
# }
# 
# plotInstanceFeatures(subsetData, "COCOsubsetDataFunctionOptimisation", c(0.85, 0.9, 0.95, 0.975))
# subsetDataFunctionOptimisation <- subsetData
# write.table(subsetData$instance, paste0("../data/availableFunctions/COCOsubsetDataFunctionOptimisation.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)




# Next COCO functions but with variying budget of 10d
#dataNewInstance10DBudget <- combineAndProcessResults("COCOinterestingFunctionsBudget10", 1, 1354)
#plotInstanceFeatures(dataNewInstance10DBudget, "newInstancesAnalysisSpacedSubset10d", c(0.85, 0.9, 0.95, 0.975))
#assessModelPerformance(dataNewInstance10DBudget, "newInstancesAnalysisSpacedSubset10d", c(0.85, 0.9, 0.95, 0.975), refData)


# 
# # Code which extracts "interesting" subset of COCO instances to run krig and cokrig on
# #bigData <- combineAndAverageResults("COCOinstanceFeatures", 1, 999, 202)
# # "Trim" big data by removing non sensical functions (i.e. functions 8, 9, 17, 18, 19 with dimension 1)
# bigDataTrimmed <- bigData[bigData$relativeError > 0.01 | bigData$globalCorrelation > 0.01, ] 
# plotInstanceFeatures(bigDataTrimmed, "bigData", c(0.85, 0.9, 0.95, 0.975))
# 
# started <- FALSE
# for(corrTop in seq(1, -0.05, -0.05)){
#   for(prob in seq(0, 1, 0.05)){
#     for(dim in c(1, 2, 5)){
#       print(paste0("Working on corr ", corrTop, " prob ", prob, " and dim ", dim))
#       tempData <- bigDataTrimmed[bigDataTrimmed$globalCorrelation < corrTop & bigDataTrimmed$globalCorrelation >= corrTop - 0.05 &
#                                    bigDataTrimmed$localCorrelationAbs_corr0.975 >= prob & bigDataTrimmed$localCorrelationAbs_corr0.975 < prob + 0.05, ]
# 
#       # mirrorData <- combined[currentData$globalCorrelation < corrTop & currentData$globalCorrelation >= corrTop - 0.05 &
#       #                          currentData$localCorrelationAbs_corr0.975 >= prob & currentData$localCorrelationAbs_corr0.975 < prob + 0.05, ]
#       # mirrorData <- mirrorData[str_which(tempData$instance, paste0("-dim", dim, "-"), negate = FALSE), ]
#       tempData <- tempData[str_which(tempData$instance, paste0("-dim", dim, "-"), negate = FALSE), ]
#       # Grab a random one if there are multiple
#       if(nrow(tempData) == 0){next}
#       tempData <- tempData[sample(1:nrow(tempData), 1), ]
#       if(!started){
#         subsetData <- tempData
#         started <- TRUE
#       }
#       else{subsetData <- rbind(subsetData, tempData)}
#     }
#   }
# }
# plotInstanceFeatures(subsetData, "bigDataSubset", c(0.85, 0.9, 0.95, 0.975))
# # Sort them by dimension
# dim1Data <- subsetData[str_which(subsetData$instance, "-dim1-", negate = FALSE),]
# dim2Data <- subsetData[str_which(subsetData$instance, "-dim2-", negate = FALSE),]
# dim5Data <- subsetData[str_which(subsetData$instance, "-dim5-", negate = FALSE),]
# sortedData <- rbind(dim1Data, dim2Data, dim5Data)
# write.table(sortedData$instance, paste0("../data/availableFunctions/COCOInterestingFunctions.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
# write.table(dim1Data$instance, paste0("../data/availableFunctions/COCOInterestingFunctionsd1.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
# write.table(dim2Data$instance, paste0("../data/availableFunctions/COCOInterestingFunctionsd2.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
# write.table(dim5Data$instance, paste0("../data/availableFunctions/COCOInterestingFunctionsd5.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)





# 
# 
# data <- combineArrayResults("modelPerformanceSetup", 1, 832)
# data <- getAveragePerformanceWithAnalysis("modelPerformanceSetup", 1, 832)
# processedFeatureData <- plotInstanceFeatures(data, "literatureAnalysis", c(0.85, 0.9, 0.95, 0.975))
# processedPerformanceData <- assessModelPerformance(data, "literatureAnalysis", c(0.85, 0.9, 0.95, 0.975))
# # Now onto varying budget of 5d
# data <- getAveragePerformanceWithAnalysis("modelAccuracyBudget5d", 1, 926)
# processedPerformanceData <- assessModelPerformance(data, "varyingBudgetLiteratureAnalysis", c(0.85, 0.9, 0.95, 0.975))
# 
# 
# # Next plot results of COCO instances
# # First plot graph with features of obtained instances
# # Note can use combine array results as there should only be one entry per function
# # Commenting out as takes forever
# #data <- combineArrayResults("COCOinstanceFeatures", 1, 494, 175)
# #processedFeatureData <- plotInstanceFeatures(data, "newInstancesAnalysis", c(0.85, 0.9, 0.95, 0.975))
# 
# # Note the script below was called to get a subset of instances, which then was used in cloudScriptCreator to create a cluster run
# # Once the run is completed, should be able to look at features
# data <- getAveragePerformanceWithAnalysis("COCOinterestingFunctionsd1d5", 1, 964)
# processedFeatureData <- plotInstanceFeatures(data, "newInstancesAnalysisSpacedSubset", c(0.85, 0.9, 0.95, 0.975))
# processedPerformanceData <- assessModelPerformance(data, "newInstancesAnalysisSpacedSubset", c(0.85, 0.9, 0.95, 0.975))
# #dataSub <- data[str_which(data$instance, "-dim1-", negate = FALSE),]
# #dataSub <- data[union(union(str_which(data$instance, "-amp0.1", negate = FALSE) , str_which(data$instance, "-amp0.6", negate = FALSE)) , str_which(data$instance, "-amp1.1", negate = FALSE)),]
# #currentData <- assessModelPerformance(dataSub, "newInstancesLiteratureAnalysis", c(0.85, 0.9, 0.95, 0.975))
# 
# data <- getAveragePerformanceWithAnalysis("COCOinterestingFunctionsd1d5budget10", 1, 964)
# processedFeatureData <- plotInstanceFeatures(data, "newInstancesAnalysisBudget10SpacedSubset", c(0.85, 0.9, 0.95, 0.975))
# processedPerformanceData <- assessModelPerformance(data, "newInstancesAnalysisBudget10SpacedSubset", c(0.85, 0.9, 0.95, 0.975))
# 
# 
# 
# 
# # averageDataAnalysed <- getAveragePerformanceWithAnalysis("COCOinterestingFunctionsd1d5", 1, 964)
# # processedPerformanceData <- assessModelPerformance(averageDataAnalysed, "AAtest", c(0.85, 0.9, 0.95, 0.975))
# # 
# # 
# # 
# # dataAverage <- getAveragePerformance("COCOinterestingFunctionsd1d5", 1, 964)
# # dataCombined <- combineArrayResults("COCOinterestingFunctionsd1d5", 1, 964)
# # dataAverage <- performAnalysis(dataCombined, dataAverage, 80, 85)
# # 
# # 
# # 
# # 
# # 
# # inst <- dataAverage[1, "instance"]
# # inst <- as.character(inst)
# # 
# # krigVals <- dataCombined[dataCombined$instance == inst & dataCombined$technique == "kriging", "performance"]
# # cokrigVals <- dataCombined[dataCombined$instance == inst & dataCombined$technique == "cokriging", "performance"]
# # 
# # result <- wilcox.test(krigVals, cokrigVals, alternative = "l")
# # result2 <- wilcox.test(cokrigVals, krigVals, alternative = "l")
# # result$p.value
# # result2$p.value
# # 
# # plot_ly(processedPerformanceData, 
# #         x = ~globalCorrelation, 
# #         y = ~localCorrelationSD, 
# #         z = ~localCorrelationAbs_corr0.975, 
# #         color = ~cokrigGoodPerformance,
# #         colors = c('red', 'black'))
#   
# # This is a script which graphs a subset of those instances and stores them in a new file
# # Want to get a subset of instances, in interesting regions
# # started <- FALSE
# # for(corrTop in seq(1, -0.95, -0.05)){
# #   for(prob in seq(0, 1, 0.05)){
# #     for(dim in c(1, 5, 10)){
# #       tempData <- currentData[currentData$globalCorrelation < corrTop & currentData$globalCorrelation >= corrTop - 0.05 &
# #                              currentData$localCorrelationAbs_corr0.975 >= prob & currentData$localCorrelationAbs_corr0.975 < prob + 0.05, ]
# #       
# #       mirrorData <- combined[currentData$globalCorrelation < corrTop & currentData$globalCorrelation >= corrTop - 0.05 &
# #                                currentData$localCorrelationAbs_corr0.975 >= prob & currentData$localCorrelationAbs_corr0.975 < prob + 0.05, ]
# #       mirrorData <- mirrorData[str_which(tempData$instance, paste0("-dim", dim, "-"), negate = FALSE), ]
# #       tempData <- tempData[str_which(tempData$instance, paste0("-dim", dim, "-"), negate = FALSE), ]
# #       # Grab a random one if there are multiple
# #       if(nrow(tempData) == 0){next}
# #       mirrorData <- mirrorData[sample(1:nrow(mirrorData), 1), ]
# #       if(!started){
# #         subsetData <- mirrorData
# #         started <- TRUE  
# #       }
# #       else{subsetData <- rbind(subsetData, mirrorData)}
# #     }    
# #   }
# # }
# # 
# # currentDataSubset <- plotInstanceFeatures(subsetData, "newInstancesAnalysisSpacedSubset", c(0.85, 0.9, 0.95, 0.975))
# # lookData <- currentDataSubset[, c("instance", "globalCorrelation", "localCorrelation_corr0.975")]
# # # Sort them by dimension
# # dim1Data <- currentDataSubset[str_which(currentDataSubset$instance, "-dim1-", negate = FALSE),]
# # dim5Data <- currentDataSubset[str_which(currentDataSubset$instance, "-dim5-", negate = FALSE),]
# # dim10Data <- currentDataSubset[str_which(currentDataSubset$instance, "-dim10-", negate = FALSE),]
# # sortedData <- rbind(dim1Data, dim5Data, dim10Data)
# # write.table(sortedData$instance, paste0("../data/availableFunctions/COCOInterestingFunctions.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
# # write.table(dim1Data$instance, paste0("../data/availableFunctions/COCOInterestingFunctionsd1.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
# # write.table(dim5Data$instance, paste0("../data/availableFunctions/COCOInterestingFunctionsd5.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
# # write.table(dim10Data$instance, paste0("../data/availableFunctions/COCOInterestingFunctionsd10.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
# 




# Code to condense cluster results into a single file for tidiness
# Although probably not needed to be honest
# combineAndSaveResults("00-finalModelAccuracySuiteSurrogateModelFit", 1, 1143, 5)
# combineAndSaveResults("01-litSuiteSurrogateModelFit", 1, 906)
# 
# resultsOld <- combineAndProcessResults("00-finalModelAccuracySuiteSurrogateModelFit", 1, 1143, 5, localCorrs = corrs)
# resultsNew <- readAndProcessResults("00-finalModelAccuracySuiteSurrogateModelFit", localCorrs = corrs)





