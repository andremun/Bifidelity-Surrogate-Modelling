source("R_code/libraries.R")

# Functions which take experimental results and create decision trees. 
# The average accuracy of these trees is printed, as well as the baseline 
# techniques of always choosing to use Co-Kriging, and a Majority Rule.
# For each set of features used, a final decision tree is built and plotted,
# and a plot of the feature importances is also created. 
# The defaults are for each leaf to contain at most 5% of the total training data,
# and setting cp = 0 allows for the tree to split as long as accuracy is improved.
# As a default the average accuracy of 100 trees is taken and printed. 
performDecisionTreePrediction <- function(data, features, iterations, runName, minPercentage, cp){
  cat(paste0("Working on ", runName))
  bad <- "Should not use Co-Kriging" 
  good <- "Can use Co-Kriging"
  predictionData <- data
  predictionData$prediction <- good
  predictionData[predictionData$superiorModel == "Kriging", "prediction"] <- bad
  
  
  performance <- 0
  if(length(features) > 1 | (features[[1]] != "alwaysCoKrig" & features[[1]] != "majorityRule")){
    for(feature in features){
      predictionData[, feature] <- as.numeric(predictionData[, feature])
    }
    importances <- data.frame("feature" = features)
    importances$importance <- 0
  }
  for(i in 1:iterations){
    cat(paste0("\rWorking on iteration ", i, "/", iterations))
    set.seed(i)
    trainIndex  <- sample(1:nrow(predictionData), 0.8 * nrow(predictionData))
    trainData <- predictionData[trainIndex,]
    testData <- predictionData[-trainIndex,]
    minBucket <- round(nrow(trainData) * minPercentage)
    
    if(length(features) > 1 | (features[[1]] != "alwaysCoKrig" & features[[1]] != "majorityRule")){
      model <- rpart(prediction ~., data = trainData[, c(features, "prediction")], control = rpart.control(minbucket = minBucket, cp = cp))
      t_pred = predict(model, testData, type = "class")
      performance <- performance + mean(testData$prediction == t_pred)
      newImportances <- as.data.frame(model$variable.importance)
      for(feature in rownames(newImportances)){
        importances[importances$feature == feature, "importance"] <- importances[importances$feature == feature, "importance"] + newImportances[feature, 1]
      }
      cat(paste0("\rWorking on iteration ", i, "/", iterations, ", current average is ", performance/i))
    }else if(features[[1]] == "majorityRule"){
      if(nrow(trainData[trainData$prediction == good, ]) > nrow(trainData[trainData$prediction == bad, ])){guess <- good}
      else{guess <- bad}
      performance <- performance + mean(testData$prediction == guess)
      cat(paste0("\rWorking on iteration ", i, "/", iterations, ", current average is ", performance/i))
    }else{
      guess <- good
      performance <- performance + mean(testData$prediction == guess)
      cat(paste0("\rWorking on iteration ", i, "/", iterations, ", current average is ", performance/i))
    }
  }
  minBucket <- round(nrow(predictionData) * minPercentage)
  cat(paste0("\rFor ", runName, ", average accuracy of ", iterations, " random runs is ", performance/iterations *100, "%"), "\n")
  
  if(length(features) > 1 | (features[[1]] != "alwaysCoKrig" & features[[1]] != "majorityRule")){
    model <- rpart(prediction ~., data = predictionData[, c(features, "prediction")], control = rpart.control(minbucket = minBucket, cp = cp))
    if(sum(model$frame$var == "<leaf>") > 1){
      pdf(paste0("data/plots/predictionPlots/", runName, "-minPercentage", minPercentage, "-default-cp", cp, ".pdf"), width = 5, height = 3)
      fancyRpartPlot(model, sub="", digits=3)
      dev.off()
    }
    t_pred = predict(model, predictionData, type = "class")
    
    # Here plot the importance. 
    importances <- data.frame("importance" = importances$importance / iterations)
    rownames(importances) <- parse(text = features)
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

    ggsave(paste0("data/plots/predictionPlots/", runName, "-minPercentage", minPercentage, "-default-cp", cp, "-variableImportance.png"), width = 16, height = 9, units = "cm")
  }
  
  return(performance/iterations)
}


runDecisionTreeAnalysis <- function(data, dataName, iterations = 100, minPercentage = 0.05, cp = 0.0){
  for(featuresIndex in c(1, 2, 3, 4, 5)){
    if(featuresIndex == 1){
      features <- c("alwaysCoKrig")
      featuresName <- "always-use-Co-Kriging"
    
    }else if(featuresIndex == 2){
      features <- c("majorityRule")
      featuresName <- "Majority-Rule"
    
    }else if(featuresIndex == 3){
      features <- c("CC")
      featuresName <- "(1)-CC_only"
    
    }else if(featuresIndex == 4){
      features <- c("dimension", "budget", "CC", "RRMSE")
      featuresName <- "(2)-literatureFeatures"
      
    }else if(featuresIndex == 5){
      features <- c("dimension", "budget", "CC", "RRMSE", "LCC_sd", "LCC_coeff",
                      "LCC_0.1", "LCC_0.2", "LCC_0.3", "LCC_0.4", "LCC_0.5", 
                      "LCC_0.6", "LCC_0.7", "LCC_0.8", "LCC_0.9", "LCC_0.95", "LCC_0.975")
      featuresName <- "(3)-allFeatures"
    }
    performDecisionTreePrediction(data, features, iterations, paste0(dataName, "_", featuresName), minPercentage, cp)
  }
}
