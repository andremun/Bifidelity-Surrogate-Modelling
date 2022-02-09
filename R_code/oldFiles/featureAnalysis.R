library("stringr")
source("arrayJobCombiner.R")


calculate_probability_sd <- function(data, name){
  col <- vector(mode = "numeric", nrow(data))
  for(i in 1:nrow(data)){
    #print(paste0(name, "prob", prob, "-", i, "/", nrow(data)))
    bins <- as.numeric(strsplit(gsub('[() ]', '', as.character(data[i, name])), ";")[[1]])
    # Caluclate width of bins
    width <- 2.0 / length(bins)
    limit <- -1
    mean <- 0
    squaredMean <- 0
    for(j in 1:length(bins)){
      midPoint <- limit + width / 2
      mean <- mean + midPoint * bins[j]
      squaredMean <- squaredMean + midPoint * midPoint * bins[j]
      limit <- limit + width
    }
    col[[i]] <- sqrt(squaredMean - mean * mean)
  }
  return(col)
}

calculate_probability_neg <- function(data, name, prob){
  #print(name)
  col <- vector(mode = "numeric", nrow(data))
  for(i in 1:nrow(data)){
    #print(paste0(name, "prob", prob, "-", i, "/", nrow(data)))
    bins <- as.numeric(strsplit(gsub('[() ]', '', as.character(data[i, name])), ";")[[1]])
    # Caluclate width of bins
    width <- 2.0 / length(bins)
    limit <- -1
    probSum <- 0
    for(j in 1:length(bins)){
      limit <- limit + width
      if(-limit < prob && abs(-limit - prob) > 0.001){break}
      probSum <- probSum + bins[j]
    }
    col[[i]] <- probSum
  }
  return(col)
}

calculate_probability <- function(data, name, prob){
  #print(name)
  col <- vector(mode = "numeric", nrow(data))
  for(i in 1:nrow(data)){
    #print(paste0(name, "prob", prob, "-", i, "/", nrow(data)))
    bins <- as.numeric(strsplit(gsub('[() ]', '', as.character(data[i, name])), ";")[[1]])
    # Caluclate width of bins
    width <- 2.0 / length(bins)
    limit <- 1
    probSum <- 0
    for(j in length(bins):1){
      limit <- limit - width
      if(limit < prob && abs(limit - prob) > 0.001){break}
      probSum <- probSum + bins[j]
    }
    col[[i]] <- probSum
  }
  return(col)
}

calculate_probability_absolute <- function(data, name, prob){
  return(calculate_probability_neg(data, name, prob) + calculate_probability(data, name, prob))
}




# Again want to make a function out of this
plotInstanceFeatures <- function(data, dataName, corrs){
  
  data$localCorrelation <- as.character(data$localCorrelation)
  data$instance <- as.character(data$instance)
  processedData <- data.frame(matrix(ncol = 6, nrow = 0))
  accepted <- 0
  
  for(instance in unique(data$instance)){
    tempData <- data[data$instance == instance, ]
    tempData <- tempData[1,]
    accepted <- accepted + 1
    processedData[accepted, ] <- tempData[, c("instance", "globalCorrelation","lowFiLowCorrelation", "highFiLowCorrelation", "relativeError", "localCorrelation")]
  }
  
  colnames(processedData) <- c("instance", "globalCorrelation","lowFiLowCorrelation", "highFiLowCorrelation", "relativeError", "localCorrelation")
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
  # Time to create all the graphs
  processedData$pch = 1
  processedData[str_which(processedData$instance, "Wang", negate = FALSE), "pch"] = 2
  processedData[str_which(processedData$instance, "Toal", negate = FALSE), "pch"] = 0
  # First feature analysis
  png(paste0("../data/analysis/features/", dataName, "ComparisonGlobalCorrVsWeightedLowFiCorr.png"), width = 682, height = 682)
  plot(c(), c(),
       col = "black",
       type = "p",
       ylab = "Global correlation",
       xlab = "Weighted correlation",
       ylim = c(-1.1, 1.1),
       xlim = c(-1.1, 1.1),
       # xaxt = "n",
       pch = 20)
  points(processedData[processedData$pch == 0, "lowFiLowCorrelation"], processedData[processedData$pch == 0, "globalCorrelation"], pch = 0, col = "black")
  points(processedData[processedData$pch == 1, "lowFiLowCorrelation"], processedData[processedData$pch == 1, "globalCorrelation"], pch = 1, col = "black")
  points(processedData[processedData$pch == 2, "lowFiLowCorrelation"], processedData[processedData$pch == 2, "globalCorrelation"], pch = 2, col = "black")
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
  legend("topleft", legend = c("Non-parametric, non-analysed instances", "Parametric, non-analysed instances", "Parametric, analysed instances"), pch = c(1, 2, 0), col = "black")
  out <- dev.off()
  
  
  
  png(paste0("../data/analysis/features/", dataName, "ComparisonGlobalCorrVsWeightedHighFiCorr.png"), width = 682, height = 682)
  plot(c(), c(),
       col = "black",
       type = "p",
       ylab = "Global correlation",
       xlab = "Weighted correlation",
       ylim = c(-1.1, 1.1),
       xlim = c(-1.1, 1.1),
       # xaxt = "n",
       pch = 20)
  points(processedData[processedData$pch == 0, "highFiLowCorrelation"], processedData[processedData$pch == 0, "globalCorrelation"], pch = 0, col = "black")
  points(processedData[processedData$pch == 1, "highFiLowCorrelation"], processedData[processedData$pch == 1, "globalCorrelation"], pch = 1, col = "black")
  points(processedData[processedData$pch == 2, "highFiLowCorrelation"], processedData[processedData$pch == 2, "globalCorrelation"], pch = 2, col = "black")
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
  legend("topleft", legend = c("Non-parametric, non-analysed instances", "Parametric, non-analysed instances", "Parametric, analysed instances"), pch = c(1, 2, 0), col = "black")
  out <- dev.off()
  
  
  
  for(corr in corrs){
    # png(paste0("../data/analysis/features/", dataName, "ComparisonGlobalCorrVsLocalCorr-", corr, ".png"), width = 682, height = 682)
    # plot(c(), c(),
    #      col = "black",
    #      type = "p",
    #      ylab = "Global Correlation",
    #      xlab = paste0("Probability local correlation > ", corr),
    #      ylim = c(-1.1, 1.1),
    #      xlim = c(-0.1, 1.1),
    #      # xaxt = "n",
    #      pch = 20)
    # points(processedData[processedData$pch == 0, paste0("localCorrelation_corr", corr)], processedData[processedData$pch == 0, "globalCorrelation"], pch = 0, col = "black")
    # points(processedData[processedData$pch == 1, paste0("localCorrelation_corr", corr)], processedData[processedData$pch == 1, "globalCorrelation"], pch = 1, col = "black")
    # points(processedData[processedData$pch == 2, paste0("localCorrelation_corr", corr)], processedData[processedData$pch == 2, "globalCorrelation"], pch = 2, col = "black")
    # lines(c(-0.1, 1.1), c(corr, corr), type = "l", lty = "longdash")
    # legend("bottomright", legend = c("Non-parametric, non-analysed instances", "Parametric, non-analysed instances", "Parametric, analysed instances"), pch = c(1, 2, 0), col = "black")
    # out <- dev.off()
    # 
    # png(paste0("../data/analysis/features/", dataName, "ComparisonGlobalCorrVsLocalCorrNeg-", corr, ".png"), width = 682, height = 682)
    # plot(c(), c(),
    #      col = "black",
    #      type = "p",
    #      ylab = "Global Correlation",
    #      xlab = paste0("Probability local correlation > ", corr),
    #      ylim = c(-1.1, 1.1),
    #      xlim = c(-0.1, 1.1),
    #      # xaxt = "n",
    #      pch = 20)
    # points(processedData[processedData$pch == 0, paste0("localCorrelationNeg_corr", corr)], processedData[processedData$pch == 0, "globalCorrelation"], pch = 0, col = "black")
    # points(processedData[processedData$pch == 1, paste0("localCorrelationNeg_corr", corr)], processedData[processedData$pch == 1, "globalCorrelation"], pch = 1, col = "black")
    # points(processedData[processedData$pch == 2, paste0("localCorrelationNeg_corr", corr)], processedData[processedData$pch == 2, "globalCorrelation"], pch = 2, col = "black")
    # lines(c(-0.1, 1.1), c(corr, corr), type = "l", lty = "longdash")
    # legend("bottomright", legend = c("Non-parametric, non-analysed instances", "Parametric, non-analysed instances", "Parametric, analysed instances"), pch = c(1, 2, 0), col = "black")
    # out <- dev.off()
    
    png(paste0("../data/analysis/features/", dataName, "ComparisonGlobalCorrVsLocalCorrAbs-", corr, ".png"), width = 682, height = 682)
    plot(c(), c(),
         col = "black",
         type = "p",
         ylab = "Global Correlation",
         xlab = paste0("Probability local correlation > ", corr),
         ylim = c(-1.1, 1.1),
         xlim = c(-0.1, 1.1),
         # xaxt = "n",
         pch = 20)
    points(processedData[processedData$pch == 0, paste0("localCorrelationAbs_corr", corr)], processedData[processedData$pch == 0, "globalCorrelation"], pch = 0, col = "black")
    points(processedData[processedData$pch == 1, paste0("localCorrelationAbs_corr", corr)], processedData[processedData$pch == 1, "globalCorrelation"], pch = 1, col = "black")
    points(processedData[processedData$pch == 2, paste0("localCorrelationAbs_corr", corr)], processedData[processedData$pch == 2, "globalCorrelation"], pch = 2, col = "black")
    lines(c(-0.1, 1.1), c(corr, corr), type = "l", lty = "longdash")
    legend("bottomright", legend = c("Non-parametric, non-analysed instances", "Parametric, non-analysed instances", "Parametric, analysed instances"), pch = c(1, 2, 0), col = "black")
    out <- dev.off()
    
    
    png(paste0("../data/analysis/features/", dataName, "ComparisonLocalCorrSDVsLocalCorrAbs-", corr, ".png"), width = 682, height = 682)
    plot(c(), c(),
         col = "black",
         type = "p",
         ylab = "Local correlation SD",
         xlab = paste0("Probability local correlation > ", corr),
         ylim = c(-0.1, 1.1),
         xlim = c(-0.1, 1.1),
         # xaxt = "n",
         pch = 20)
    points(processedData[processedData$pch == 0, paste0("localCorrelationAbs_corr", corr)], processedData[processedData$pch == 0, "localCorrelationSD"], pch = 0, col = "black")
    points(processedData[processedData$pch == 1, paste0("localCorrelationAbs_corr", corr)], processedData[processedData$pch == 1, "localCorrelationSD"], pch = 1, col = "black")
    points(processedData[processedData$pch == 2, paste0("localCorrelationAbs_corr", corr)], processedData[processedData$pch == 2, "localCorrelationSD"], pch = 2, col = "black")
    legend("bottomright", legend = c("Non-parametric, non-analysed instances", "Parametric, non-analysed instances", "Parametric, analysed instances"), pch = c(1, 2, 0), col = "black")
    out <- dev.off()
  }
  
  png(paste0("../data/analysis/features/", dataName, "ComparisonGlobalCorrVsRelativeError.png"), width = 682, height = 682)
  plot(c(), c(),
       col = "black",
       type = "p",
       ylab = "Global correlation",
       xlab = "Relative error",
       ylim = c(-1.1, 1.1),
       xlim = c(-0.1, 1.1 * max(processedData$relativeError)),
       # xaxt = "n",
       pch = 20)
  points(processedData[processedData$pch == 0, "relativeError"], processedData[processedData$pch == 0, "globalCorrelation"], pch = 0, col = "black")
  points(processedData[processedData$pch == 1, "relativeError"], processedData[processedData$pch == 1, "globalCorrelation"], pch = 1, col = "black")
  points(processedData[processedData$pch == 2, "relativeError"], processedData[processedData$pch == 2, "globalCorrelation"], pch = 2, col = "black")
  legend("topright", legend = c("Non-parametric, non-analysed instances", "Parametric, non-analysed instances", "Parametric, analysed instances"), pch = c(1, 2, 0), col = "black")
  out <- dev.off()
  
  png(paste0("../data/analysis/features/", dataName, "ComparisonGlobalCorrVsLocalCorrSD.png"), width = 682, height = 682)
  plot(c(), c(),
       col = "black",
       type = "p",
       ylab = "Global correlation",
       xlab = "Local correlation SD",
       ylim = c(-1.1, 1.1),
       xlim = c(-0.1, 1.1),
       # xaxt = "n",
       pch = 20)
  points(processedData[processedData$pch == 0, "localCorrelationSD"], processedData[processedData$pch == 0, "globalCorrelation"], pch = 0, col = "black")
  points(processedData[processedData$pch == 1, "localCorrelationSD"], processedData[processedData$pch == 1, "globalCorrelation"], pch = 1, col = "black")
  points(processedData[processedData$pch == 2, "localCorrelationSD"], processedData[processedData$pch == 2, "globalCorrelation"], pch = 2, col = "black")
  legend("topright", legend = c("Non-parametric, non-analysed instances", "Parametric, non-analysed instances", "Parametric, analysed instances"), pch = c(1, 2, 0), col = "black")
  out <- dev.off()
  
  return(processedData)
}

#data <- getAveragePerformance("modelPerformanceSetup", 1, 832)
#currentData <- plotInstanceFeatures(data, "literatureAnalysis", c(0.85, 0.9, 0.95, 0.975))


