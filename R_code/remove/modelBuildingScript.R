library(rpart)
library(rattle)
library(rpart.plot)
library(caret)
library(RColorBrewer)
library(MLmetrics)
library(dplyr)
source("dataProcessor.R")

performPrediction <- function(data, predictionType, features, iterations, runName, minPercentage, accuracyRep, cp = 0.1, doOpt = FALSE){
  
  #minPercentage <- 0.02
  #print(paste0("Min percentage is ", minPercentage, ", cp is", cp))
  
  cat(paste0("Working on ", runName))
  if(substr(predictionType, 1, 1) == "k"){
    good <- "Should not use Co-Kriging" 
    bad <- "Can use Co-Kriging"
  }else{
    good <- "Should not use Kriging"
    bad <- "Can use Kriging"
  }
  
  predictionData <- data
  predictionData$prediction <- good
  predictionData[predictionData$bestPerformanceWilcoxon != predictionType, "prediction"] <- bad
  
  #as.factor(predictionData$bestPerformanceWilcoxon == predictionType)
  
  for(feature in features){
    predictionData[, feature] <- as.numeric(predictionData[, feature])
  }
  performance <- 0
  performanceOptimised <- 0
  
  if(length(features) > 0){
    importances <- data.frame("feature" = features)
    importances$importance <- 0
  }
  for(i in 1:iterations){
    cat(paste0("\rWorking on iteration ", i, "/", iterations))
    set.seed(i)
    trainIndex  <- sample(1:nrow(predictionData), 0.8 * nrow(predictionData))
    # trainIndex <- createDataPartition(predictionData$prediction, p = .8, 
    #                                   list = FALSE, 
    #                                   times = 1)
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

      # t_predOpt <- predict(modelOpt, testData)
      
      # if(sum(modelOpt$finalModel$frame$var == "<leaf>") > 1){
      #   pdf(paste0("../data/analysis/decisionTrees/", runName, "-seed", i, "-minPercentage", minPercentage, "-entropy.pdf"))
      #   fancyRpartPlot(modelOpt$finalModel)
      #   dev.off()
      # }
      if(doOpt){
        modelOpt <- train(trainData[, features], trainData$prediction,
                          method = "rpart",
                          tuneLength = 50,
                          metric = "Accuracy",
                          trControl = trainControl(method = "repeatedcv",
                                                   number = accuracyRep,
                                                   repeats = accuracyRep,
                                                   summaryFunction = multiClassSummary,
                                                   classProbs = TRUE),
                          control = rpart.control(minbucket = minBucket),
                          parms = list(split = "gini"))
  
        t_predOpt <- predict(modelOpt, testData)
        
        if(sum(modelOpt$finalModel$frame$var == "<leaf>") > 1){
          pdf(paste0("../data/analysis/decisionTrees/", runName, "-seed", i, "-minPercentage", minPercentage, "-gini.pdf"))
          fancyRpartPlot(modelOpt$finalModel)
          dev.off()
        }
        performanceOptimised <- performanceOptimised + mean(testData$prediction == t_predOpt)
      }
      
      model <- rpart(prediction ~., data = trainData[, c(features, "prediction")], control = rpart.control(minbucket = minBucket, cp = cp), parms = list(split = "gini"))
      t_pred = predict(model, testData, type = "class")
      performance <- performance + mean(testData$prediction == t_pred)
      newImportances <- as.data.frame(model$variable.importance)
      for(feature in rownames(newImportances)){
        importances[importances$feature == feature, "importance"] <- importances[importances$feature == feature, "importance"] + newImportances[feature, 1]
      }
      
      
      #print(paste0("Best param is ", modelOpt$bestTune$cp))
      if(doOpt){cat(paste0("\rWorking on iteration ", i, "/", iterations, ", current average is ", performance/i, ", current opt average is ", performanceOptimised/i))}
      else{cat(paste0("\rWorking on iteration ", i, "/", iterations, ", current average is ", performance/i))}
    }else{
      if(nrow(trainData[trainData$prediction == good, ]) > nrow(trainData[trainData$prediction == bad, ])){guess <- good}
      else{guess <- bad}
      guess <- bad
      #guess <- nrow(trainData[trainData$prediction == TRUE, ]) > nrow(trainData[trainData$prediction == FALSE, ])
      performance <- performance + mean(testData$prediction == guess)
      cat(paste0("\rWorking on iteration ", i, "/", iterations, ", current average is ", performance/i))
    }
    
  }
  minBucket <- round(nrow(predictionData) * minPercentage)
  if(doOpt){cat(paste0("\rFor ", runName, ", average performance of ", iterations, " random runs is ", performance/iterations), ", opt average performance is ", performanceOptimised/iterations, " with minPercentage", minPercentage)}
  else{cat(paste0("\rFor ", runName, ", average performance of ", iterations, " random runs is ", performance/iterations), " with minPercentage", minPercentage)}
  if(length(features) > 0){
    
    # model <- rpart(prediction ~., data = predictionData[, c(features, "prediction")])
    if(doOpt){
      model <- train(predictionData[, features], predictionData$prediction,
                     method = "rpart",
                     tuneLength = 50,
                     metric = "Accuracy",
                     trControl = trainControl(method = "repeatedcv",
                                              number = accuracyRep,
                                              repeats = accuracyRep,
                                              summaryFunction = multiClassSummary,
                                              classProbs = TRUE),
                     control = rpart.control(minbucket = minBucket),
                     parms = list(split = "gini"))
  
      t_predOpt <- predict(model, predictionData[, features])
    
      #png(paste0("../data/analysis/decisionTrees/", runName, ".png"), width = 1600, height = 1200)
      if(sum(model$finalModel$frame$var == "<leaf>") > 1){
        pdf(paste0("../data/analysis/decisionTrees/", runName, "-minPercentage", minPercentage, "-gini.pdf"))
        fancyRpartPlot(model$finalModel, sub = "")
        dev.off()
      }
    }
    
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
    
    #png(paste0("../data/analysis/decisionTrees/", runName, ".png"), width = 1600, height = 1200)
    # if(sum(model$finalModel$frame$var == "<leaf>") > 1){
    #   pdf(paste0("../data/analysis/decisionTrees/", runName, "-minPercentage", minPercentage, "-entropy.pdf"))
    #   fancyRpartPlot(model$finalModel)
    #   dev.off()
    # }
    
    
    
    model <- rpart(prediction ~., data = predictionData[, c(features, "prediction")], control = rpart.control(minbucket = minBucket, cp = cp), parms = list(split = "gini"))
    if(sum(model$frame$var == "<leaf>") > 1){
      pdf(paste0("../data/analysis/decisionTrees/", runName, "-minPercentage", minPercentage, "-default-cp", cp, ".pdf"), width = 5, height = 3)
      fancyRpartPlot(model, sub="", digits=3)
      dev.off()
    }
    t_pred = predict(model, predictionData, type = "class")
    if(doOpt){cat(", final accuracy of ", mean(predictionData$prediction == t_pred), ", and optimal final accuracy of ", mean(predictionData$prediction == t_predOpt), "\n")}
    else{cat(", final accuracy of ", mean(predictionData$prediction == t_pred), "\n")}
    
    # Here plot the importance. Want to understand what it stands for though!
    importances <- data.frame("importance" = importances$importance / iterations)
    rownames(importances) <- parse(text = features)
    #importances <- slice_max(importances, importances$importance, n=20)
    df2 <- importances %>%
            tibble::rownames_to_column() %>% 
            dplyr::rename("feature" = rowname) %>%
            dplyr::arrange(importance) %>%
            dplyr::mutate(feature = forcats::fct_inorder(feature))
    ggplot2::ggplot(df2) +
      geom_segment(aes(x = feature, y = 0, xend = feature, yend = importance),
                   size = 1.5, alpha = 0.7) +
      geom_point(aes(x = feature, y = importance, col = "black"),
                 size = 3, show.legend = F) +
      coord_flip() +
      theme_bw() +
      xlab("Feature") + 
      ylab("Importance")
      # theme(axis.title.x=element_blank(),
      #       axis.title.y=element_blank())

    ggsave(paste0("../data/analysis/decisionTrees/", runName, "-minPercentage", minPercentage, "-default-cp", cp, "-variableImportance.png"), width = 16, height = 9, units = "cm")
    return(importances)
    # for(feature in importances$feature){
    #   print(paste0("Feature ", feature, " has an average importance of ", importances[importances$feature == feature, "importance"]/iterations))
    # }
    # print(importances)
  }else{
    cat("\n")
  }
  
  
}


runAnalysis <- function(data, dataName, iterations, minPercentage, accuracyRep, cp = 0.1, krigName = "kriging_1_0", coKrigName = "cokriging_0.8_0.2", doOpt = FALSE){
  
  for(technique in c(1)){
    if(technique == 1){
      techniqueName <- "avoidCokrig"
      techniqueTechName <- krigName
    }else{
      techniqueName <- "avoidKrig"
      techniqueTechName <- coKrigName
    }
    # for(dataIndex in 1:3){
    #   if(dataIndex == 1){
    #     dataName <- "litData"
    #     data <- dataLitInstance5DBudget
    #   }else if(dataIndex == 2){
    #     dataName <- "newData"
    #     data <- dataNewInstance5DBudget
    #   }else{
    #     dataName <- "allData"
    #     data <- allData
    #   }
      
    for(featuresIndex in c(1, 2, 3, 14)){
      if(featuresIndex == 1){
        features <- c()
        featuresName <- "guess"
      
      }else if(featuresIndex == 2){
        features <- c("globalCorrelation_R2")
        featuresName <- "(1)globalCorr"
      
      }else if(featuresIndex == 3){
        #features <- c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError")
        features <- c("dimension", "budget", "globalCorrelation_R2", "relativeError")
        featuresName <- "(2)litFeat_R2"
        
      }else if(featuresIndex == 4){
        features <- c("dimension", "budget", "costRatio", "globalCorrelation", "globalCorrelation_R2", "relativeError")
        featuresName <- "(3)litFeat_all"
        
      }else if(featuresIndex == 5){
        features <- c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "localCorrelationAbs_corr0.975", "localCorrelationSD", "localCorrelationCoefficientOfVariation", "highFiLowCorrelation")
        featuresName <- "(4)litAndBasicNewFeat"
      
      }else if(featuresIndex == 6){
        features <- c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError", "localCorrelationR2_corr0.95", "localCorrelationSD_R2", "localCorrelationCoefficientOfVariation_R2", "highFiLowCorrelation_R2")
        featuresName <- "(5)litAndBasicNewFeat_R2"
        
      }else if(featuresIndex == 7){
        features <- c("dimension", "budget", "costRatio", "globalCorrelation", "globalCorrelation_R2", "relativeError", "localCorrelationAbs_corr0.975", "localCorrelationR2_corr0.95", "localCorrelationSD", "localCorrelationSD_R2", "highFiLowCorrelation", "highFiLowCorrelation_R2")
        featuresName <- "(6)litAndBasicNewFeat_all"
        
      }else if(featuresIndex == 8){
        features <-  c("dimension", "budget", "costRatio", "globalCorrelation", "relativeError", "localCorrelationAbs_corr0.85", "localCorrelationAbs_corr0.9", "localCorrelationAbs_corr0.95", "localCorrelationAbs_corr0.975", "localCorrelationCoefficientOfVariation", "localCorrelationSD", "highFiLowCorrelation")
        featuresName <- "(7)AllFeat"
      
      }else if(featuresIndex == 9){
        #features <-  c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError", "localCorrelationR2_corr0.85", "localCorrelationR2_corr0.9", "localCorrelationR2_corr0.95", "localCorrelationR2_corr0.975", "localCorrelationCoefficientOfVariation_R2", "localCorrelationSD_R2")#, "highFiLowCorrelation_R2")
        features <-  c("dimension", "budget", "globalCorrelation_R2", "relativeError",
                       #"localCorrelationR2_corr0.05", "localCorrelationR2_corr0.1", "localCorrelationR2_corr0.15", "localCorrelationR2_corr0.2", 
                       #"localCorrelationR2_corr0.25", "localCorrelationR2_corr0.3", "localCorrelationR2_corr0.35", "localCorrelationR2_corr0.4", "localCorrelationR2_corr0.45", 
                       #"localCorrelationR2_corr0.5", "localCorrelationR2_corr0.55", "localCorrelationR2_corr0.6", "localCorrelationR2_corr0.65", 
                       #"localCorrelationR2_corr0.7", "localCorrelationR2_corr0.75", "localCorrelationR2_corr0.8", 
                       "localCorrelationR2_corr0.85", "localCorrelationR2_corr0.9", "localCorrelationR2_corr0.95", "localCorrelationR2_corr0.975", 
                       "localCorrelationCoefficientOfVariation_R2", "localCorrelationSD_R2")#, "highFiLowCorrelation_R2")
        featuresName <- "(8)AllFeat_R2_original"
      
      }else if(featuresIndex == 10){
        features <-  c("dimension", "budget", "costRatio", "globalCorrelation", "globalCorrelation_R2", "relativeError", 
                       "localCorrelation_corr0.85", "localCorrelationNeg_corr0.85", "localCorrelationAbs_corr0.85", "localCorrelationR2_corr0.85", 
                       "localCorrelation_corr0.9", "localCorrelationNeg_corr0.9", "localCorrelationAbs_corr0.9", "localCorrelationR2_corr0.9", 
                       "localCorrelation_corr0.95", "localCorrelationNeg_corr0.95", "localCorrelationAbs_corr0.95", "localCorrelationR2_corr0.95", 
                       "localCorrelation_corr0.975", "localCorrelationNeg_corr0.975", "localCorrelationAbs_corr0.975", "localCorrelationR2_corr0.975", 
                       "localCorrelationCoefficientOfVariation_R2", "localCorrelationCoefficientOfVariation",
                       "localCorrelationSD", "localCorrelationSD_R2",
                       "highFiLowCorrelation", "highFiLowCorrelation_R2")
        featuresName <- "(9)AllFeat_all"  
      
      }else if(featuresIndex == 11){
        features <-  c("dimension", "budget", "costRatio", "globalCorrelation", "globalCorrelation_R2", "relativeError", 
                       "localCorrelation_corr0.85", "localCorrelationNeg_corr0.85", "localCorrelationAbs_corr0.85", "localCorrelationR2_corr0.85", 
                       "localCorrelation_corr0.9", "localCorrelationNeg_corr0.9", "localCorrelationAbs_corr0.9", "localCorrelationR2_corr0.9", 
                       "localCorrelation_corr0.95", "localCorrelationNeg_corr0.95", "localCorrelationAbs_corr0.95", "localCorrelationR2_corr0.95", 
                       "localCorrelation_corr0.975", "localCorrelationNeg_corr0.975", "localCorrelationAbs_corr0.975", "localCorrelationR2_corr0.975", 
                       "localCorrelationCoefficientOfVariation_R2", "localCorrelationCoefficientOfVariation",
                       "localCorrelationSD", "localCorrelationSD_R2",
                       "highFiLowCorrelation", "highFiLowCorrelation_R2",
                       "lowFiLowCorrelation", "lowFiLowCorrelation_R2")
        featuresName <- "(10)AllFeatExtra_all"  
      
      }else if(featuresIndex == 12){
        features <-  c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError", "localCorrelationR2_corr0.85", "localCorrelationR2_corr0.9", "localCorrelationR2_corr0.95", "localCorrelationR2_corr0.975",
                       "localCorrelationCoefficientOfVariation_R2", "localCorrelationSD_R2", "highFiLowCorrelation_R2", "lowFiLowCorrelation_R2")
        
        featuresName <- "(11)AllFeatExtra_all_R2"  
      
      }else if(featuresIndex == 13){
        #features <-  c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError", "localCorrelationR2_corr0.85", "localCorrelationR2_corr0.9", "localCorrelationR2_corr0.95", "localCorrelationR2_corr0.975", "localCorrelationCoefficientOfVariation_R2", "localCorrelationSD_R2")#, "highFiLowCorrelation_R2")
        features <-  c("dimension", "budget", "globalCorrelation_R2", "relativeError",
                       "localCorrelationR2_corr0.05", "localCorrelationR2_corr0.15", 
                       "localCorrelationR2_corr0.25", "localCorrelationR2_corr0.35", "localCorrelationR2_corr0.45", 
                       "localCorrelationR2_corr0.55", "localCorrelationR2_corr0.65", 
                       "localCorrelationR2_corr0.75", "localCorrelationR2_corr0.85", "localCorrelationR2_corr0.95",
                       "localCorrelationCoefficientOfVariation_R2", "localCorrelationSD_R2")#, "highFiLowCorrelation_R2")
        featuresName <- "(12)AllFeat_R2_reduced"
      
      }else if(featuresIndex == 14){
        #features <-  c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError", "localCorrelationR2_corr0.85", "localCorrelationR2_corr0.9", "localCorrelationR2_corr0.95", "localCorrelationR2_corr0.975", "localCorrelationCoefficientOfVariation_R2", "localCorrelationSD_R2")#, "highFiLowCorrelation_R2")
        features <-  c("dimension", "budget", "globalCorrelation_R2", "relativeError",
                       "localCorrelationR2_corr0.1", "localCorrelationR2_corr0.2", "localCorrelationR2_corr0.3", "localCorrelationR2_corr0.4", 
                       "localCorrelationR2_corr0.5", "localCorrelationR2_corr0.6", "localCorrelationR2_corr0.7", "localCorrelationR2_corr0.8", 
                       "localCorrelationR2_corr0.9", "localCorrelationR2_corr0.95", "localCorrelationR2_corr0.975",
                       "localCorrelationCoefficientOfVariation_R2", "localCorrelationSD_R2")#, "highFiLowCorrelation_R2")
        featuresName <- "(13)AllFeat_R2_reducedOther"
      }
      
      
      # Replace for display purposes
      oldNames <- c("relativeError", "globalCorrelation_R2", "localCorrelationCoefficientOfVariation_R2", "localCorrelationSD_R2")
      #newNames <- c("RRMSE", "CC", as.expression(bquote(LCC*textstyle(atop(0.2, "coeff")))), as.expression(bquote(LCC*textstyle(atop(0.2, "sd")))))
      newNames <- c("RRMSE", "CC", "LCC_coeff", "LCC_sd")
      for(corr in c(seq(0.05, 0.95, 0.05), 0.975)){
        oldNames <- c(oldNames, paste0("localCorrelationR2_corr", corr))
        #newNames <- c(newNames, as.expression(bquote(LCC*textstyle(atop(0.2, .(corr))))))
        newNames <- c(newNames, paste0("LCC_", corr))
      }
      for(i in 1:length(oldNames)){
        oldName <- oldNames[[i]]
        newName <- newNames[[i]]
        colnames(data)[colnames(data) == oldName] <- newName
        features[features == oldName] <- newName
      }
      #print(data)
      performPrediction(data, techniqueTechName, features, iterations, paste0(techniqueName, "_", dataName, "_", featuresName), minPercentage, accuracyRep, cp = cp, doOpt = doOpt)
      
    }
    cat("\n")
   # }
  }
  
}

#dataset01_litSuiteSurrogateModelFit <- combineAndProcessResults("01-litSuiteSurrogateModelFit", 1, 906)
#dataset02a_litSuiteSurrogateModelExploration <- combineAndProcessResults("02a-litSuiteSurrogateModelExploration", 1, 686)
#dataset02b_litSuiteSurrogateModelExploration <- combineAndProcessResults("02b-litSuiteSurrogateModelExploration", 1, 2360)
#dataset03a_litSuiteSurrogateModelExploitation <- combineAndProcessResults("03a-litSuiteSurrogateModelExploitation", 1, 686)
#dataset03b_litSuiteSurrogateModelExploitation <- combineAndProcessResults("03b-litSuiteSurrogateModelExploitation", 1, 2360)
#dataset04_modelAccuracySuiteSurrogateModelFit <- combineAndProcessResults("04-modelAccuracySuiteSurrogateModelFit", 1, 1098, 6)
#dataset05a_modelAccuracySuiteSurrogateModelExploration <- combineAndProcessResults("05a-modelAccuracySuiteSurrogateModelExploration", 1, 4208)
#dataset05b_modelAccuracySuiteSurrogateModelExploration <- combineAndProcessResults("05b-modelAccuracySuiteSurrogateModelExploration", 1, 15600)
#dataset06a_modelAccuracySuiteSurrogateModelExploitation <- combineAndProcessResults("06a-modelAccuracySuiteSurrogateModelExploitation", 1, 4208)
#dataset06b_modelAccuracySuiteSurrogateModelExploitation <- combineAndProcessResults("06b-modelAccuracySuiteSurrogateModelExploitation", 1, 15600)
#dataset07_functionOptSuiteSurrogateModelFit <- combineAndProcessResults("07-functionOptSuiteSurrogateModelFit", 1, 1083, 3)
#dataset08a_functionOptSuiteSurrogateModelExploration <- combineAndProcessResults("08a-functionOptSuiteSurrogateModelExploration", 1, 2102)
#dataset08b_functionOptSuiteSurrogateModelExploration <- combineAndProcessResults("08b-functionOptSuiteSurrogateModelExploration", 1, 7400)
#dataset09a_functionOptSuiteSurrogateModelExploitation <- combineAndProcessResults("09a-functionOptSuiteSurrogateModelExploitation", 1, 2102)
#dataset09b_functionOptSuiteSurrogateModelExploitation <- combineAndProcessResults("09b-functionOptSuiteSurrogateModelExploitation", 1, 7400)





#runAnalysis(dataset01_litSuiteSurrogateModelFit, "01-litSuiteSurrogateModelFit", reps, minPerc, optReps, cp = cp, krigName = "kriging_1_0", coKrigName = "cokriging_0.8_0.2")
#runAnalysis(dataset02a_litSuiteSurrogateModelExploration, "02-litSuiteSurrogateModelExploration", reps, minPerc, optReps, cp = cp,  krigName = "kriging_0.5_0", coKrigName = "cokriging_0.4_0.1")
#runAnalysis(dataset03a_litSuiteSurrogateModelExploitation, "03-lsa itSuiteSurrogateModelExploitation", reps, minPerc, optReps, cp = cp,  krigName = "kriging_0.5_0", coKrigName = "cokriging_0.4_0.1")
#runAnalysis(dataset04_modelAccuracySuiteSurrogateModelFit, "04-modelAccuracySuiteSurrogateModelFit", reps, minPerc, optReps, cp = cp,  krigName = "kriging_1_0", coKrigName = "cokriging_0.8_0.2")
#runAnalysis(dataset05a_modelAccuracySuiteSurrogateModelExploration, "05-modelAccuracySuiteSurrogateModelExploration", reps, minPerc, optReps, cp = cp,  krigName = "kriging_0.5_0", coKrigName = "cokriging_0.4_0.1")
#runAnalysis(dataset06a_modelAccuracySuiteSurrogateModelExploitation, "06-modelAccuracySuiteSurrogateModelExploitation", reps, minPerc, optReps, cp = cp,  krigName = "kriging_0.5_0", coKrigName = "cokriging_0.4_0.1")
#runAnalysis(dataset07_functionOptSuiteSurrogateModelFit, "07-functionOptSuiteSurrogateModelFit", reps, minPerc, optReps, cp = cp,  krigName = "kriging_1_0", coKrigName = "cokriging_0.8_0.2")
#runAnalysis(dataset08a_functionOptSuiteSurrogateModelExploration, "08-functionOptSuiteSurrogateModelExploration", reps, minPerc, optReps, cp = cp,  krigName = "kriging_0.5_0", coKrigName = "cokriging_0.4_0.1")
#runAnalysis(dataset09a_functionOptSuiteSurrogateModelExploitation, "09-functionOptSuiteSurrogateModelExploitation", reps, minPerc, optReps, cp = cp,  krigName = "kriging_0.5_0", coKrigName = "cokriging_0.4_0.1")

#enhanced_10 <- rbind(dataset01_litSuiteSurrogateModelFit, dataset04_modelAccuracySuiteSurrogateModelFit)
#enhanced_11 <- rbind(dataset02a_litSuiteSurrogateModelExploration, dataset05a_modelAccuracySuiteSurrogateModelExploration)
#enhanced_12 <- rbind(dataset03a_litSuiteSurrogateModelExploitation, dataset09a_functionOptSuiteSurrogateModelExploitation)
#runAnalysis(enhanced_10, "10-enhancedSurrogateModelFit", reps, minPerc, optReps, cp = cp,  krigName = "kriging_1_0", coKrigName = "cokriging_0.8_0.2")
#runAnalysis(enhanced_11, "11-enhancedSurrogateModelExploration", reps, minPerc, optReps, cp = cp,  krigName = "kriging_0.5_0", coKrigName = "cokriging_0.4_0.1")
#runAnalysis(enhanced_12, "12-enhancedSurrogateModelExploitation", reps, minPerc, optReps, cp = cp,  krigName = "kriging_0.5_0", coKrigName = "cokriging_0.4_0.1")


# I ASSUME THIS IS THE FINAL WORKING

#dataset01_litSuiteSurrogateModelFit <- combineAndProcessResults("01-litSuiteSurrogateModelFit", 1, 906, localCorrs = corrs)
#dataset02_finalModelAccuracySuiteSurrogateModelFit <- combineAndProcessResults("00-finalModelAccuracySuiteSurrogateModelFit", 1, 1164, 5, localCorrs = corrs)

# minPerc <- 0.025
# cp <- 0
# optReps <- 10
# reps <- 1
# 
# for(index in c(1)){
#   cp <- c(0, 0.05, 0.1)[[index]]
#   minPerc <- c(0.05, 0, 0)[[index]]
#   reps <- 100
#   optReps <- 4
#   doOpt <- FALSE
#   runAnalysis(dataset01_litSuiteSurrogateModelFit, "01-litSuiteSurrogateModelFit", reps, minPerc, optReps, cp = cp, krigName = "kriging_1_0", coKrigName = "cokriging_0.8_0.2", doOpt = doOpt)
#   
#   runAnalysis(rbind(dataset01_litSuiteSurrogateModelFit,dataset02_finalModelAccuracySuiteSurrogateModelFit), "03-enhancedSuiteSurrogateModelFit", reps, minPerc, optReps, cp = cp,  krigName = "kriging_1_0", coKrigName = "cokriging_0.8_0.2", doOpt = doOpt)
#   #runAnalysis(rbind(dataset01_litSuiteSurrogateModelFit,dataset04_newModelAccuracySuiteSurrogateModelFit), "05-newEnhancedSuiteSurrogateModelFit", reps, minPerc, optReps, cp = cp,  krigName = "kriging_1_0", coKrigName = "cokriging_0.8_0.2", doOpt = doOpt)
#   #runAnalysis(rbind(dataset01_litSuiteSurrogateModelFit, dataset02_finalModelAccuracySuiteSurrogateModelFit), "03-enhancedSuiteSurrogateModelFit", reps, minPerc, optReps, cp = cp,  krigName = "kriging_1_0", coKrigName = "cokriging_0.8_0.2", doOpt = doOpt)
#   
#   
# }





# NEWEST STUFF HERE
# Working with a bunch of new features
# litData <- read.table("../data/instanceSpaceAnalysisInput/litSuiteAllFeatures.csv", header = TRUE, sep = ",")
# 
# litData$bestPerformanceWilcoxon <- "None"
# litData[litData$algo_Kriging <= 1.05*litData$algo_Co.Kriging &
#           litData$algo_Co.Kriging > 1.05*litData$algo_Kriging, "bestPerformanceWilcoxon"] <- "kriging_1_0"
# litData[litData$algo_Co.Kriging <= 1.05*litData$algo_Kriging &
#           litData$algo_Kriging > 1.05*litData$algo_Co.Kriging, "bestPerformanceWilcoxon"] <- "cokriging_0.8_0.2"
# 
# cp <- 0
# minPerc <- 0.05
# reps <- 100
# optReps <- 4
# doOpt <- FALSE
# techniqueName <- "avoidCokrig"
# krigName <- "kriging_1_0"
# techniqueTechName <- krigName
# features <- colnames(litData)[3:136]
# performPrediction(litData, techniqueTechName, features, reps, "avoidCokrig-litData-expandedFeatures", minPerc, optReps, cp = cp, doOpt = doOpt)
# 
# 
# augData <- read.table("../data/instanceSpaceAnalysisInput/augSuiteAllFeatures.csv", header = TRUE, sep = ",")
# augData$bestPerformanceWilcoxon <- "None"
# augData[augData$algo_Kriging <= 1.05*augData$algo_Co.Kriging &
#           augData$algo_Co.Kriging > 1.05*augData$algo_Kriging, "bestPerformanceWilcoxon"] <- "kriging_1_0"
# augData[augData$algo_Co.Kriging <= 1.05*augData$algo_Kriging &
#           augData$algo_Kriging > 1.05*augData$algo_Co.Kriging, "bestPerformanceWilcoxon"] <- "cokriging_0.8_0.2"
# features <- colnames(augData)[3:136]
# performPrediction(rbind(litData, augData), techniqueTechName, features, reps, "avoidCokrig-augData-expandedFeatures", minPerc, optReps, cp = cp, doOpt = doOpt)
