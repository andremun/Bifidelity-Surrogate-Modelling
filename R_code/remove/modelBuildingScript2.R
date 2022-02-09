library(rpart)
library(rattle)
library(rpart.plot)
library(caret)
library(RColorBrewer)
library(MLmetrics)
library(C50)
library("partykit")
source("dataProcessor.R")

performPrediction <- function(data, predictionType, features, iterations, runName, minPercentage, accuracyRep){
  
  #minPercentage <- 0.02
  
  cat(paste0("Working on ", runName))
  if(substr(predictionType, 1, 1) == "k"){
    good <- "CannotUseCokrig" 
    bad <- "MayUseCokrig"
  }else{
    good <- "CannotUseKrig"
    bad <- "MayUseKrig"
  }
  
  predictionData <- data
  predictionData$prediction <- good
  predictionData[predictionData$bestPerformanceWilcoxon != predictionType, "prediction"] <- bad
  predictionData$prediction <- as.factor(predictionData$prediction)
  
  #as.factor(predictionData$bestPerformanceWilcoxon == predictionType)
  
  for(feature in features){
    predictionData[, feature] <- as.numeric(predictionData[, feature])
  }
  performance <- 0
  performanceOptimised <- 0
  performanceG50 <- 0
  
  for(i in 1:iterations){
    cat(paste0("\rWorking on iteration ", i, "/", iterations))
    set.seed(i)
    #trainIndex  <- sample(1:nrow(predictionData), 0.8 * nrow(predictionData))
    trainIndex <- createDataPartition(predictionData$prediction, p = .8, 
                                      list = FALSE, 
                                      times = 1)
    trainData <- predictionData[trainIndex,]
    testData <- predictionData[-trainIndex,]
    minBucket <- round(nrow(trainData) * minPercentage)
    if(length(features) > 0){
      
      # modelOpt <- train(trainData[, features], trainData$prediction,
      #                    method = "rpart",
      #                    tuneLength = 50,
      #                    metric = "Accuracy",
      #                    trControl = trainControl(method = "repeatedcv",
      #                                             number = accuracyRep,
      #                                             repeats = accuracyRep,
      #                                             summaryFunction = multiClassSummary,
      #                                             classProbs = TRUE),
      #                   control = rpart.control(minbucket = minBucket),
      #                   parms = list(split = "entropy"))
      # 
      # t_predOpt <- predict(modelOpt, testData)
      # 
      # if(sum(modelOpt$finalModel$frame$var == "<leaf>") > 1){
      #   pdf(paste0("../data/analysis/decisionTrees/", runName, "-seed", i, "-minPercentage", minPercentage, "-entropy.pdf"))
      #   fancyRpartPlot(modelOpt$finalModel)
      #   dev.off()
      # }
      # 
      # modelOpt <- train(trainData[, features], trainData$prediction,
      #                   method = "rpart",
      #                   tuneLength = 50,
      #                   metric = "Accuracy",
      #                   trControl = trainControl(method = "repeatedcv",
      #                                            number = accuracyRep,
      #                                            repeats = accuracyRep,
      #                                            summaryFunction = multiClassSummary,
      #                                            classProbs = TRUE),
      #                   control = rpart.control(minbucket = minBucket),
      #                   parms = list(split = "gini"))
      # 
      # t_predOpt <- predict(modelOpt, testData)
      # 
      # if(sum(modelOpt$finalModel$frame$var == "<leaf>") > 1){
      #   pdf(paste0("../data/analysis/decisionTrees/", runName, "-seed", i, "-minPercentage", minPercentage, "-gini.pdf"))
      #   fancyRpartPlot(modelOpt$finalModel)
      #   dev.off()
      # }
      
      model <- rpart(prediction ~., data = trainData[, c(features, "prediction")], control = rpart.control(minbucket = minBucket, cp = 0.0001), parms = list(split = "gini"))
      t_pred = predict(model, testData, type = "class")
      performance <- performance + mean(testData$prediction == t_pred)
      if(sum(model$frame$var == "<leaf>") > 1){
        pdf(paste0("../data/analysis/decisionTrees/", runName, "-seed", i, "-minPercentage", minPercentage, "-gini.pdf"))
        fancyRpartPlot(model)
        dev.off()
      }
      
      tree <- C5.0(prediction ~., data = trainData[, c(features, "prediction")], trials = 10)
      predictions <- predict(tree, newdata = testData)
      performanceG50 <- performanceG50 + mean(testData$prediction == t_pred)
      #pdf(paste0("../data/analysis/decisionTrees/", runName, "-seed", i, "-minPercentage", minPercentage, "-G50.pdf"))
      #myTree2 <- C50:::as.party.C5.0(tree)
      #print(summary(tree))
      #dev.off()
      
      #performanceOptimised <- performanceOptimised + mean(testData$prediction == t_predOpt)
      #print(paste0("Best param is ", modelOpt$bestTune$cp))
      cat(paste0("\rWorking on iteration ", i, "/", iterations, ", current average is ", performance/i, ", average of G50 is ", performanceG50/i))
    }else{
      if(nrow(trainData[trainData$prediction == good, ]) > nrow(trainData[trainData$prediction == bad, ])){guess <- good}
      else{guess <- bad}
      #guess <- nrow(trainData[trainData$prediction == TRUE, ]) > nrow(trainData[trainData$prediction == FALSE, ])
      performance <- performance + mean(testData$prediction == guess)
      cat(paste0("\rWorking on iteration ", i, "/", iterations, ", current average is ", performance/i))
    }
    
  }
  minBucket <- round(nrow(predictionData) * minPercentage)
  cat(paste0("\rFor ", runName, ", average performance of ", iterations, " random runs is ", performance/iterations), ", average of G50 is ", performanceG50/i, ", with minPercentage", minPercentage)
  if(length(features) > 0){
    # model <- rpart(prediction ~., data = predictionData[, c(features, "prediction")])
    # model <- train(predictionData[, features], predictionData$prediction,
    #                method = "rpart",
    #                tuneLength = 50,
    #                metric = "Accuracy",
    #                trControl = trainControl(method = "repeatedcv",
    #                                         number = accuracyRep,
    #                                         repeats = accuracyRep,
    #                                         summaryFunction = multiClassSummary,
    #                                         classProbs = TRUE),
    #                control = rpart.control(minbucket = minBucket),
    #                parms = list(split = "gini"))
    # 
    # t_predOpt <- predict(model, predictionData[, features])
    # 
    # #png(paste0("../data/analysis/decisionTrees/", runName, ".png"), width = 1600, height = 1200)
    # if(sum(model$finalModel$frame$var == "<leaf>") > 1){
    #   pdf(paste0("../data/analysis/decisionTrees/", runName, "-minPercentage", minPercentage, "-gini.pdf"))
    #   fancyRpartPlot(model$finalModel)
    #   dev.off()
    # }
    # 
    # model <- train(predictionData[, features], predictionData$prediction,
    #                method = "rpart",
    #                tuneLength = 50,
    #                metric = "Accuracy",
    #                trControl = trainControl(method = "repeatedcv",
    #                                         number = accuracyRep,
    #                                         repeats = accuracyRep,
    #                                         summaryFunction = multiClassSummary,
    #                                         classProbs = TRUE),
    #                control = rpart.control(minbucket = minBucket),
    #                parms = list(split = "entropy"))
    # 
    # #png(paste0("../data/analysis/decisionTrees/", runName, ".png"), width = 1600, height = 1200)
    # if(sum(model$finalModel$frame$var == "<leaf>") > 1){
    #   pdf(paste0("../data/analysis/decisionTrees/", runName, "-minPercentage", minPercentage, "-entropy.pdf"))
    #   fancyRpartPlot(model$finalModel)
    #   dev.off()
    # }
    # 
    
    
    model <- rpart(prediction ~., data = predictionData[, c(features, "prediction")], control = rpart.control(minbucket = minBucket, cp = 0.0001), parms = list(split = "gini"))
    if(sum(model$frame$var == "<leaf>") > 1){
      pdf(paste0("../data/analysis/decisionTrees/", runName, "-minPercentage", minPercentage, "-default.pdf"))
      fancyRpartPlot(model)
      dev.off()
    }
    t_pred = predict(model, testData, type = "class")
    cat(", final accuracy of ", mean(testData$prediction == t_pred), "\n")
    
    tree <- C5.0(prediction ~., data = predictionData[, c(features, "prediction")], trials = 10)
  
  }else{
    cat("\n")
  }
  
  
}


runAnalysis <- function(iterations, minPercentage, accuracyRep){
  
  for(technique in 1:2){
    if(technique == 1){
      techniqueName <- "avoidCokrig"
      techniqueTechName <- "kriging_1_0"
    }else{
      techniqueName <- "avoidKrig"
      techniqueTechName <- "cokriging_0.8_0.2"
    }
    for(dataIndex in 1:3){
      if(dataIndex == 1){
        dataName <- "litData"
        data <- dataLitInstance5DBudget
      }else if(dataIndex == 2){
        dataName <- "newData"
        data <- dataNewInstance5DBudget
      }else{
        dataName <- "allData"
        data <- allData
      }
      
      for(featuresIndex in 1:7){
        if(featuresIndex == 1){
          features <- c()
          featuresName <- "guess"
        
        }else if(featuresIndex == 2){
          features <- c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError")
          featuresName <- "(1)litFeat"
        
        }else if(featuresIndex == 3){
          features <- c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError")
          featuresName <- "(2)litFeat_R2"
          
        }else if(featuresIndex == 4){
          features <- c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "localCorrelationAbs_corr0.975", "localCorrelationSD")
          featuresName <- "(3)litAndBasicNewFeat"
        
        }else if(featuresIndex == 5){
          features <- c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError", "localCorrelationR2_corr0.95", "localCorrelationSD_R2")
          featuresName <- "(4)litAndBasicNewFeat_R2"
          
        }else if(featuresIndex == 6){
          features <-  c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "localCorrelationAbs_corr0.85", "localCorrelationAbs_corr0.9", "localCorrelationAbs_corr0.95", "localCorrelationAbs_corr0.975", "localCorrelationCoefficientOfVariation", "localCorrelationSD")
          featuresName <- "(5)AllFeat"
        
        }else if(featuresIndex == 7){
          features <-  c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError", "localCorrelationR2_corr0.85", "localCorrelationR2_corr0.9", "localCorrelationR2_corr0.95", "localCorrelationR2_corr0.975", "localCorrelationCoefficientOfVariation_R2", "localCorrelationSD_R2")
          featuresName <- "(6)AllFeat_R2"
        }
        # }else if(featuresIndex == 2){
        #   features <- c("globalCorrelation", "relativeError")
        #   featuresName <- "(1)litBasicNewNoneFeat"
        # 
        # }else if(featuresIndex == 3){
        #   features <- c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError")
        #   featuresName <- "(2)litAllNewNoneFeat"
        #   
        # }else if(featuresIndex == 4){
        #   features <- c("globalCorrelation", "relativeError", "localCorrelationAbs_corr0.975", "localCorrelationVariance")
        #   featuresName <- "(3)litBasicNewBasicFeat"
        #   
        # }else if(featuresIndex == 5){
        #   features <- c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "localCorrelationAbs_corr0.975", "localCorrelationVariance")
        #   featuresName <- "(4)litAllNewBasicFeat"
        # 
        # }else if(featuresIndex == 6){
        #   features <- c("globalCorrelation", "relativeError", "lowFiLowCorrelation", "highFiLowCorrelation", "localCorrelationAbs_corr0.85", "localCorrelationAbs_corr0.9", "localCorrelationAbs_corr0.95", "localCorrelationAbs_corr0.975", "localCorrelationCoefficientOfVariation", "localCorrelationVariance")
        #   featuresName <- "(5)litBasicNewAllFeat"
        #   
        # }else if(featuresIndex == 7){
        #   features <- c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "lowFiLowCorrelation", "highFiLowCorrelation", "localCorrelationAbs_corr0.85", "localCorrelationAbs_corr0.9", "localCorrelationAbs_corr0.95", "localCorrelationAbs_corr0.975", "localCorrelationCoefficientOfVariation", "localCorrelationVariance")
        #   featuresName <- "(6)litAllNewAllFeat"
        #   
        # }
        
        performPrediction(data, techniqueTechName, features, iterations, paste0(techniqueName, "_", dataName, "_", featuresName), minPercentage, accuracyRep)
        
      }
      cat("\n")
    }
  }
  
}
# 
# performPrediction(dataLitInstance5DBudget, "kriging_1_0", c("globalCorrelation", "relativeError"), 50, "krig_litData_litBasicNewNoneFeat")
# performPrediction(dataLitInstance5DBudget, "kriging_1_0", c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError"), 50, "krig_litData_litAllNewNoneFeat")
# performPrediction(dataLitInstance5DBudget, "kriging_1_0", c("globalCorrelation", "relativeError", "localCorrelationAbs_corr0.975", "localCorrelationVariance"), 50, "krig_litData_litBasicNewBasicFeat")
# performPrediction(dataLitInstance5DBudget, "kriging_1_0", c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "localCorrelationAbs_corr0.975", "localCorrelationVariance"), 50, "krig_litData_litAllNewBasicFeat")
# performPrediction(dataLitInstance5DBudget, "kriging_1_0", c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "lowFiLowCorrelation", "highFiLowCorrelation", "localCorrelationAbs_corr0.85", "localCorrelationAbs_corr0.9", "localCorrelationAbs_corr0.95", "localCorrelationAbs_corr0.975", "localCorrelationCoefficientOfVariation", "localCorrelationVariance"), 50, "krig_litData_litAllNewAllFeat")
# print("")
# performPrediction(dataNewInstance5DBudget, "kriging_1_0", c("globalCorrelation", "relativeError"), 50, "krig_newData_litBasicNewNoneFeat")
# performPrediction(dataNewInstance5DBudget, "kriging_1_0", c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError"), 50, "krig_newData_litAllNewNoneFeat")
# performPrediction(dataNewInstance5DBudget, "kriging_1_0", c("globalCorrelation", "relativeError", "localCorrelationAbs_corr0.975", "localCorrelationVariance"), 50, "krig_newData_litBasicNewBasicFeat")
# performPrediction(dataNewInstance5DBudget, "kriging_1_0", c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "localCorrelationAbs_corr0.975", "localCorrelationVariance"), 50, "krig_newData_litAllNewBasicFeat")
# performPrediction(dataNewInstance5DBudget, "kriging_1_0", c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "lowFiLowCorrelation", "highFiLowCorrelation", "localCorrelationAbs_corr0.85", "localCorrelationAbs_corr0.9", "localCorrelationAbs_corr0.95", "localCorrelationAbs_corr0.975", "localCorrelationCoefficientOfVariation", "localCorrelationVariance"), 50, "krig_newData_litAllNewAllFeat")
# print("")
# performPrediction(allData, "kriging_1_0", c("globalCorrelation", "relativeError"), 50, "krig_allData_litBasicNewNoneFeat")
# performPrediction(allData, "kriging_1_0", c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError"), 50, "krig_allData_litAllNewNoneFeat")
# performPrediction(allData, "kriging_1_0", c("globalCorrelation", "relativeError", "localCorrelationAbs_corr0.975", "localCorrelationVariance"), 50, "krig_allData_litBasicNewBasicFeat")
# performPrediction(allData, "kriging_1_0", c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "localCorrelationAbs_corr0.975", "localCorrelationVariance"), 50, "krig_allData_litAllNewBasicFeat")
# performPrediction(allData, "kriging_1_0", c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "lowFiLowCorrelation", "highFiLowCorrelation", "localCorrelationAbs_corr0.85", "localCorrelationAbs_corr0.9", "localCorrelationAbs_corr0.95", "localCorrelationAbs_corr0.975", "localCorrelationCoefficientOfVariation", "localCorrelationVariance"), 50, "krig_allData_litAllNewAllFeat")


# 
#dataLitInstance5DBudget <- combineAndProcessResults("modelAccuracyBudget5d", 1, 926)
#dataNewInstance5DBudget <- combineAndProcessResults("COCOinterestingFunctionsBudget5", 1, 1354)
#allData <- rbind(dataLitInstance5DBudget, dataNewInstance5DBudget)
# 
# # Ok try to train decision tree
# # First specify data to use
# #predictionData <- dataLitInstance5DBudget
# predictionData <- dataNewInstance5DBudget
# #predictionData <- rbind(dataNewInstance5DBudget, dataLitInstance5DBudget)
# 
# # Then choose what we are trying to guess
# # Maybe start with when only kriging is good
# predictionData$prediction <- as.factor(predictionData$bestPerformanceWilcoxon == "kriging_1_0")
# #predictionData$prediction <- as.factor(predictionData$bestPerformanceWilcoxon == "cokriging_0.8_0.2")
# 
# # Specify features
# #features <- c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError")
# features <- c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "lowFiLowCorrelation", "highFiLowCorrelation", "localCorrelationAbs_corr0.85", "localCorrelationAbs_corr0.9", "localCorrelationAbs_corr0.95", "localCorrelationAbs_corr0.975", "localCorrelationCoefficientOfVariation", "localCorrelationVariance")
# #features <- c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "localCorrelationAbs_corr0.975", "localCorrelationVariance")
# 
# for(feature in features){
#   predictionData[, feature] <- as.numeric(predictionData[, feature])
# }
# 
# # Split off data
# trainIndex  <- sample(1:nrow(predictionData), 0.8 * nrow(predictionData))
# trainData <- predictionData[trainIndex,]
# testData <- predictionData[-trainIndex,]
# 
# # Train model
# model <- rpart(prediction ~., data = trainData[, c(features, "prediction")])
# 
# #t_pred = predict(tree, testData, type="class")
# t_pred = predict(model, testData, type = "class")
# mean(testData$prediction == t_pred)
# #plot(model)
# #text(model, digits = 3)
# png("../data/analysis/decisionTrees/test.png", width = 800, height = 600)
# rpart.plot(model)
# dev.off()
# # plot mytree
# #fancyRpartPlot(model, caption = NULL)
# 
#dataLitInstance5DBudget <- combineAndProcessResults("modelAccuracyBudget5d", 1, 926)
#dataNewInstance5DBudget <- combineAndProcessResults("COCOinterestingFunctionsBudget5", 1, 1354)
#allData <- rbind(dataLitInstance5DBudget, dataNewInstance5DBudget)
runAnalysis(20, 0.049, 10)
#runAnalysis(20, 0, 10)
#runAnalysis(20, 0.025, 10)
#runAnalysis(20, 0.05, 10)


# predictionData <- dataNewInstance5DBudget
# predictionData$prediction <- "best"
# predictionData[predictionData$bestPerformanceWilcoxon != "cokriging_0.8_0.2", "prediction"] <- "notBest"
# features <- c("globalCorrelation", "relativeError", "lowFiLowCorrelation", "highFiLowCorrelation", "localCorrelationAbs_corr0.85", "localCorrelationAbs_corr0.9", "localCorrelationAbs_corr0.95", "localCorrelationAbs_corr0.975", "localCorrelationCoefficientOfVariation", "localCorrelationVariance")
# 
# #as.factor(predictionData$bestPerformanceWilcoxon == predictionType)
# 
# for(feature in features){
#   predictionData[, feature] <- as.numeric(predictionData[, feature])
# }
# model <- rpart(prediction ~., data = predictionData[, c(features, "prediction")])
# model
# 
# sum(model$frame$var == "<leaf>")
# print(model)
