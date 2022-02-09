library("stringr")
source("featureAnalysis.R")
source("arrayJobCombiner.R")

# Again want to make a function out of this
assessModelPerformance <- function(data, dataName, corrs){
  
  data$localCorrelation <- as.character(data$localCorrelation)
  data$instance <- as.character(data$instance)
  
  # Grab all the function names
  processedData <- data.frame(matrix(ncol = 6, nrow = 0))
  skipped <- 0
  accepted <- 0
  
  for(instance in unique(data$instance)){
    if(nrow(data[data$instance == instance & data$technique == "kriging", ]) != 1 |
            nrow(data[data$instance == instance & data$technique == "cokriging", ]) != 1){
              skipped <- skipped + 1
              print(paste0("Skipped instance ", instance))
              next
    }
    
    if(data[data$instance == instance & data$technique == "kriging", "globalCorrelation"] -
       data[data$instance == instance & data$technique == "cokriging", "globalCorrelation"] > 0.001){
      print(paste0("Something weird with instance ", instance, " function analysis is different for cokriging and kriging"))
      print(data[data$instance == instance & data$technique == "kriging", c("globalCorrelation","lowFiLowCorrelation", "highFiLowCorrelation", "relativeError") ] -
              data[data$instance == instance & data$technique == "cokriging", c("globalCorrelation","lowFiLowCorrelation", "highFiLowCorrelation", "relativeError") ])
    }
    
    accepted <- accepted + 1
    processedData[accepted, ] <- data[data$instance == instance & data$technique == "cokriging", c("instance", "globalCorrelation","lowFiLowCorrelation", "highFiLowCorrelation", "relativeError", "localCorrelation")]
  }
  
  colnames(processedData) <- c("instance", "globalCorrelation","lowFiLowCorrelation", "highFiLowCorrelation", "relativeError", "localCorrelation")
  print(paste0("Accepted ", accepted, " instances, rejected ", skipped, " instances"))
  
  # Process local correlation
  for(corr in corrs){
    name <- paste0("localCorrelation_corr", corr)
    processedData[, name] <- calculate_probability(processedData, "localCorrelation", corr)
  }
  for(corr in corrs){
    name <- paste0("localCorrelationNeg_corr", corr)
    processedData[, name] <- calculate_probability_neg(processedData, "localCorrelation", corr)
  }
  for(corr in corrs){
    name <- paste0("localCorrelationAbs_corr", corr)
    processedData[, name] <- calculate_probability_absolute(processedData, "localCorrelation", corr)
  }
  processedData[, "localCorrelationSD"] <- calculate_probability_sd(processedData, "localCorrelation")
  
  # Can now drop local correlation
  processedData <- processedData[, -6]
  # Now for each budget, grab performance
  for(i in 1:nrow(processedData)){
    instance <- processedData[i, "instance"]
    processedData[i, "krigPerformance"] <- data[data$instance == instance & data$technique == "kriging", "performance"]
    processedData[i, "cokrigPerformance"] <- data[data$instance == instance & data$technique == "cokriging", "performance"]
    if("willcoxKrigGood" %in% colnames(data)){
      processedData[i, "krigGoodPerformanceWillcox"] <- data[data$instance == instance & data$technique == "kriging", "willcoxKrigGood"]
      processedData[i, "cokrigGoodPerformanceWillcox"] <- data[data$instance == instance & data$technique == "cokriging", "willcoxCoKrigGood"]
    }
  }
  # Now work out best
  processedData$bestPerformance <- pmin(processedData$krigPerformance, processedData$cokrigPerformance, na.rm=TRUE)
  processedData$krigGoodPerformanceMean <- processedData$bestPerformance * 1.05 > processedData$krigPerformance & !is.na(processedData$krigPerformance)
  processedData$cokrigGoodPerformanceMean <- processedData$bestPerformance * 1.05 > processedData$cokrigPerformance & !is.na(processedData$cokrigPerformance)
  
  # Now the comparison of performance
  for(performanceMeasurePrefix in c("Mean", "Willcox")){
    for(technique in c("krig", "cokrig")){
      for(printType in c("both", "badOnly", "goodOnly")){
        if(printType == "both"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-PerformanceGlobalCorrVsWeightedLowFiCorr.png"), width = 682, height = 682)}
        else if(printType == "badOnly"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-BadOnly-PerformanceGlobalCorrVsWeightedLowFiCorr.png"), width = 682, height = 682)}
        else{png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-GoodOnly-PerformanceGlobalCorrVsWeightedLowFiCorr.png"), width = 682, height = 682)}
        plot(c(), c(),
             col = "black",
             type = "p",
             ylab = "Global correlation",
             xlab = "Weighted correlation",
             ylim = c(-1.1, 1.1),
             xlim = c(-1.1, 1.1),
             # xaxt = "n",
             pch = 20)
        if(printType != "badOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, "lowFiLowCorrelation"], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, "globalCorrelation"], pch = 16, col = "black")}
        if(printType != "goodOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, "lowFiLowCorrelation"], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, "globalCorrelation"], pch = 16, col = "red")}
        lines(c(-1, 1), c(-1, 1), type = "l", lty = "longdash")
        lines(c(-1, 0.85), c(-0.85, 1), type = "l", lty = "dotdash")
        lines(c(-0.85, 1), c(-1, 0.85), type = "l", lty = "dotdash")
        lines(c(-0.90, -0.90), c(-1, 1), type = "l", lty = "dotdash")
        lines(c(0.90, 0.90), c(-1, 1), type = "l", lty = "dotdash")
        lines(c(-0.95, -0.95), c(-1, 1), type = "l", lty = "dotdash")
        lines(c(0.95, 0.95), c(-1, 1), type = "l", lty = "dotdash")
        lines(c(-1, 1), c(-0.90, -0.90), type = "l", lty = "dotdash")
        lines(c(-1, 1), c(0.90, 0.90), type = "l", lty = "dotdash")
        lines(c(-1, 1), c(-0.95, -0.95), type = "l", lty = "dotdash")
        lines(c(-1, 1), c(0.95, 0.95), type = "l", lty = "dotdash")
        if(printType == "both"){legend("topleft", legend = c("Bad performance", "Good performance"), pch = c(16, 16), col = c("red", "black"))}
        else if(printType == "badOnly"){legend("topleft", legend = c("Bad performance"), pch = c(16), col = c("red"))}
        else{legend("topleft", legend = c("Good performance"), pch = c(16), col = c("black"))}
        out <- dev.off()
      
        
    
        if(printType == "both"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-PerformanceGlobalCorrVsWeightedHighFiCorr.png"), width = 682, height = 682)}
        else if(printType == "badOnly"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-BadOnly-PerformanceGlobalCorrVsWeightedHighFiCorr.png"), width = 682, height = 682)}
        else{png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-GoodOnly-PerformanceGlobalCorrVsWeightedHighFiCorr.png"), width = 682, height = 682)}
        plot(c(), c(),
             col = "black",
             type = "p",
             ylab = "Global correlation",
             xlab = "Weighted correlation",
             ylim = c(-1.1, 1.1),
             xlim = c(-1.1, 1.1),
             # xaxt = "n",
             pch = 20)
        if(printType != "badOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, "highFiLowCorrelation"], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, "globalCorrelation"], pch = 16, col = "black")}
        if(printType != "goodOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, "highFiLowCorrelation"], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, "globalCorrelation"], pch = 16, col = "red")}
        lines(c(-1, 1), c(-1, 1), type = "l", lty = "longdash")
        lines(c(-1, 0.85), c(-0.85, 1), type = "l", lty = "dotdash")
        lines(c(-0.85, 1), c(-1, 0.85), type = "l", lty = "dotdash")
        lines(c(-0.90, -0.90), c(-1, 1), type = "l", lty = "dotdash")
        lines(c(0.90, 0.90), c(-1, 1), type = "l", lty = "dotdash")
        lines(c(-0.95, -0.95), c(-1, 1), type = "l", lty = "dotdash")
        lines(c(0.95, 0.95), c(-1, 1), type = "l", lty = "dotdash")
        lines(c(-1, 1), c(-0.90, -0.90), type = "l", lty = "dotdash")
        lines(c(-1, 1), c(0.90, 0.90), type = "l", lty = "dotdash")
        lines(c(-1, 1), c(-0.95, -0.95), type = "l", lty = "dotdash")
        lines(c(-1, 1), c(0.95, 0.95), type = "l", lty = "dotdash")
        if(printType == "both"){legend("topleft", legend = c("Bad performance", "Good performance"), pch = c(16, 16), col = c("red", "black"))}
        else if(printType == "badOnly"){legend("topleft", legend = c("Bad performance"), pch = c(16), col = c("red"))}
        else{legend("topleft", legend = c("Good performance"), pch = c(16), col = c("black"))}
        out <- dev.off()
    
    
    
        for(corr in corrs){
          # if(printType == "both"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", technique, "-PerformanceGlobalCorrVsLocalCorr-", corr, ".png"), width = 682, height = 682)}
          # else if(printType == "badOnly"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", technique, "-BadOnly-PerformanceGlobalCorrVsLocalCorr-", corr, ".png"), width = 682, height = 682)}
          # else{png(paste0("../data/analysis/modelPerformance/", dataName, "-", technique, "-GoodOnly-PerformanceGlobalCorrVsLocalCorr-", corr, ".png"), width = 682, height = 682)}
          # plot(c(), c(),
          #      col = "black",
          #      type = "p",
          #      ylab = "Global Correlation",
          #      xlab = paste0("Probability local correlation > ", corr),
          #      ylim = c(-1.1, 1.1),
          #      xlim = c(-0.1, 1.1),
          #      # xaxt = "n",
          #      pch = 20)
          # if(printType != "badOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance")] == TRUE, paste0("localCorrelation_corr", corr)], processedData[processedData[, paste0(technique, "GoodPerformance")] == TRUE, "globalCorrelation"], pch = 16, col = "black")}
          # if(printType != "goodOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance")] == FALSE, paste0("localCorrelation_corr", corr)], processedData[processedData[, paste0(technique, "GoodPerformance")] == FALSE, "globalCorrelation"], pch = 16, col = "red")}
          # lines(c(-0.1, 1.1), c(corr, corr), type = "l", lty = "longdash")
          # if(printType == "both"){legend("topleft", legend = c("Bad performance", "Good performance"), pch = c(16, 16), col = c("red", "black"))}
          # else if(printType == "badOnly"){legend("topleft", legend = c("Bad performance"), pch = c(16), col = c("red"))}
          # else{legend("topleft", legend = c("Good performance"), pch = c(16), col = c("black"))}
          # out <- dev.off()
          # 
          # if(printType == "both"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", technique, "-PerformanceGlobalCorrVsLocalCorrNeg-", corr, ".png"), width = 682, height = 682)}
          # else if(printType == "badOnly"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", technique, "-BadOnly-PerformanceGlobalCorrVsLocalCorrNeg-", corr, ".png"), width = 682, height = 682)}
          # else{png(paste0("../data/analysis/modelPerformance/", dataName, "-", technique, "-GoodOnly-PerformanceGlobalCorrVsLocalCorrNeg-", corr, ".png"), width = 682, height = 682)}
          # plot(c(), c(),
          #      col = "black",
          #      type = "p",
          #      ylab = "Global Correlation",
          #      xlab = paste0("Probability local correlation < -", corr),
          #      ylim = c(-1.1, 1.1),
          #      xlim = c(-0.1, 1.1),
          #      # xaxt = "n",
          #      pch = 20)
          # if(printType != "badOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance")] == TRUE, paste0("localCorrelationNeg_corr", corr)], processedData[processedData[, paste0(technique, "GoodPerformance")] == TRUE, "globalCorrelation"], pch = 16, col = "black")}
          # if(printType != "goodOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance")] == FALSE, paste0("localCorrelationNeg_corr", corr)], processedData[processedData[, paste0(technique, "GoodPerformance")] == FALSE, "globalCorrelation"], pch = 16, col = "red")}
          # lines(c(-0.1, 1.1), c(corr, corr), type = "l", lty = "longdash")
          # if(printType == "both"){legend("topleft", legend = c("Bad performance", "Good performance"), pch = c(16, 16), col = c("red", "black"))}
          # else if(printType == "badOnly"){legend("topleft", legend = c("Bad performance"), pch = c(16), col = c("red"))}
          # else{legend("topleft", legend = c("Good performance"), pch = c(16), col = c("black"))}
          # out <- dev.off()
          
          if(printType == "both"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-PerformanceGlobalCorrVsLocalCorrAbs-", corr, ".png"), width = 682, height = 682)}
          else if(printType == "badOnly"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-BadOnly-PerformanceGlobalCorrVsLocalCorrAbs-", corr, ".png"), width = 682, height = 682)}
          else{png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-GoodOnly-PerformanceGlobalCorrVsLocalCorrAbs-", corr, ".png"), width = 682, height = 682)}
          plot(c(), c(),
               col = "black",
               type = "p",
               ylab = "Global Correlation",
               xlab = paste0("Probability abs(local correlation) > ", corr),
               ylim = c(-1.1, 1.1),
               xlim = c(-0.1, 1.1),
               # xaxt = "n",
               pch = 20)
          if(printType != "badOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, paste0("localCorrelationAbs_corr", corr)], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, "globalCorrelation"], pch = 16, col = "black")}
          if(printType != "goodOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, paste0("localCorrelationAbs_corr", corr)], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, "globalCorrelation"], pch = 16, col = "red")}
          lines(c(-0.1, 1.1), c(corr, corr), type = "l", lty = "longdash")
          if(printType == "both"){legend("topleft", legend = c("Bad performance", "Good performance"), pch = c(16, 16), col = c("red", "black"))}
          else if(printType == "badOnly"){legend("topleft", legend = c("Bad performance"), pch = c(16), col = c("red"))}
          else{legend("topleft", legend = c("Good performance"), pch = c(16), col = c("black"))}
          out <- dev.off()
          
          
          if(printType == "both"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-PerformanceLocalCorrSDVsLocalCorrAbss-", corr, ".png"), width = 682, height = 682)}
          else if(printType == "badOnly"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-BadOnly-PerformanceLocalCorrSDVsLocalCorrAbs-", corr, ".png"), width = 682, height = 682)}
          else{png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-GoodOnly-PerformanceLocalCorrSDVsLocalCorrAbs-", corr, ".png"), width = 682, height = 682)}
          plot(c(), c(),
               col = "black",
               type = "p",
               ylab = "Local correlation SD",
               xlab = paste0("Probability abs(local correlation) > ", corr),
               ylim = c(-0.1, 1.1),
               xlim = c(-0.1, 1.1),
               # xaxt = "n",
               pch = 20)
          if(printType != "badOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, paste0("localCorrelationAbs_corr", corr)], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, "localCorrelationSD"], pch = 16, col = "black")}
          if(printType != "goodOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, paste0("localCorrelationAbs_corr", corr)], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, "localCorrelationSD"], pch = 16, col = "red")}
          if(printType == "both"){legend("topleft", legend = c("Bad performance", "Good performance"), pch = c(16, 16), col = c("red", "black"))}
          else if(printType == "badOnly"){legend("topleft", legend = c("Bad performance"), pch = c(16), col = c("red"))}
          else{legend("topleft", legend = c("Good performance"), pch = c(16), col = c("black"))}
          out <- dev.off()
          
        }
        
        
        if(printType == "both"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-PerformanceGlobalCorrVsLocalError.png"), width = 682, height = 682)}
        else if(printType == "badOnly"){png(paste0("../data/analysis/modelPerformance/", dataName, performanceMeasurePrefix, "-", "-", technique, "-BadOnly-PerformanceGlobalCorrVsLocalError.png"), width = 682, height = 682)}
        else{png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-GoodOnly-PerformanceGlobalCorrVsLocalError.png"), width = 682, height = 682)}
        plot(c(), c(),
             col = "black",
             type = "p",
             ylab = "Global correlation",
             xlab = "Relative error",
             ylim = c(-1.1, 1.1),
             xlim = c(-0.1, 1.1 * max(processedData$relativeError)),
             # xaxt = "n",
             pch = 20)
        if(printType != "badOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, "relativeError"], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, "globalCorrelation"], pch = 16, col = "black")}
        if(printType != "goodOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, "relativeError"], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, "globalCorrelation"], pch = 16, col = "red")}
        if(printType == "both"){legend("topright", legend = c("Bad performance", "Good performance"), pch = c(16, 16), col = c("red", "black"))}
        else if(printType == "badOnly"){legend("topright", legend = c("Bad performance"), pch = c(16), col = c("red"))}
        else{legend("topright", legend = c("Good performance"), pch = c(16), col = c("black"))}
        out <- dev.off()
        
        if(printType == "both"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-PerformanceGlobalCorrVsLocalErrorZoom.png"), width = 682, height = 682)}
        else if(printType == "badOnly"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-BadOnly-PerformanceGlobalCorrVsLocalErrorZoom.png"), width = 682, height = 682)}
        else{png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-GoodOnly-PerformanceGlobalCorrVsLocalErrorZoom.png"), width = 682, height = 682)}
        plot(c(), c(),
             col = "black",
             type = "p",
             ylab = "Global correlation",
             xlab = "Relative error",
             ylim = c(0.5, 1.05),
             xlim = c(-0.1, 1),
             # xaxt = "n",
             pch = 20)
        if(printType != "badOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, "relativeError"], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, "globalCorrelation"], pch = 16, col = "black")}
        if(printType != "goodOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, "relativeError"], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, "globalCorrelation"], pch = 16, col = "red")}
        if(printType == "both"){legend("topright", legend = c("Bad performance", "Good performance"), pch = c(16, 16), col = c("red", "black"))}
        else if(printType == "badOnly"){legend("topright", legend = c("Bad performance"), pch = c(16), col = c("red"))}
        else{legend("topright", legend = c("Good performance"), pch = c(16), col = c("black"))}
        out <- dev.off()
        
        if(printType == "both"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-PerformanceGlobalCorrVsLocalCorrSD.png"), width = 682, height = 682)}
        else if(printType == "badOnly"){png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-BadOnly-PerformanceGlobalCorrVsLocalCorrSD.png"), width = 682, height = 682)}
        else{png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-", technique, "-GoodOnly-PerformanceGlobalCorrVsLocalCorrSD.png"), width = 682, height = 682)}
        plot(c(), c(),
             col = "black",
             type = "p",
             ylab = "Global correlation",
             xlab = "Local correlation SD",
             ylim = c(-1.1, 1.1),
             xlim = c(-0.1, 1.1),
             # xaxt = "n",
             pch = 20)
        if(printType != "badOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, "localCorrelationSD"], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == TRUE, "globalCorrelation"], pch = 16, col = "black")}
        if(printType != "goodOnly"){points(processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, "localCorrelationSD"], processedData[processedData[, paste0(technique, "GoodPerformance", performanceMeasurePrefix)] == FALSE, "globalCorrelation"], pch = 16, col = "red")}
        if(printType == "both"){legend("topright", legend = c("Bad performance", "Good performance"), pch = c(16, 16), col = c("red", "black"))}
        else if(printType == "badOnly"){legend("topright", legend = c("Bad performance"), pch = c(16), col = c("red"))}
        else{legend("topright", legend = c("Good performance"), pch = c(16), col = c("black"))}
        out <- dev.off()
      }
    }
  }
  return(processedData)
}

#dataCombined <- combineArrayResults("modelPerformanceSetup", 1, 832)
#dataCombined$instance <- as.character(dataCombined$instance)
#data <- getAveragePerformance("modelPerformanceSetup", 1, 832)
#currentData <- assessModelPerformance(data, "literatureAnalysis", c(0.85, 0.9, 0.95, 0.975))
#currentData <- plotInstanceFeatures(data, "literatureAnalysis", c(0.85, 0.9, 0.95, 0.975))



