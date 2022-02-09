library("stringr")

# Again want to make a function out of this
assessModelPerformance <- function(processedData, dataName, corrs, refData, mult = 1){
  # Now the comparison of performance
  #for(performanceMeasurePrefix in c("Mean", "Wilcoxon")){
  for(performanceMeasurePrefix in c("Wilcoxon")){
    if(performanceMeasurePrefix == "Mean"){colName <- "bestPerformanceMean"}
    else{colName <- "bestPerformanceWilcoxon"}
    
    smallDim <- 10
    
    tempData <- data.frame("CC" = as.numeric(processedData$globalCorrelation_R2),
                           "RRMSE" = as.numeric(processedData$relativeError),
                           "HFCC" = as.numeric(processedData$highFiLowCorrelation_R2),
                           "perform" = factor(processedData[, colName], levels = refData$algName))
    
    
    p <- ggplot(tempData, aes(x = RRMSE, y = CC)) +
          geom_point(aes(color = perform, shape = perform), size = 1) + 
          scale_color_manual(values = setNames(object = refData$col, refData$algName),
                             labels = setNames(object = refData$label, refData$algName),
                             name = "Best algorithm") +
          scale_shape_manual(values = setNames(object = refData$pch, refData$algName),
                             labels = setNames(object = refData$label, refData$algName),
                             name = "Best algorithm")
    ggsave(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceCCVsRRMSE.png"),
           plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
           width = mult*18, height = mult*12, units = "cm")
    
    p <- ggplot(tempData, aes(x = HFCC, y = CC)) +
          geom_point(aes(color = perform, shape = perform), size = 1) + 
          scale_color_manual(values = setNames(object = refData$col, refData$algName),
                             labels = setNames(object = refData$label, refData$algName),
                             name = "Best algorithm") +
          scale_shape_manual(values = setNames(object = refData$pch, refData$algName),
                             labels = setNames(object = refData$label, refData$algName),
                             name = "Best algorithm")
    ggsave(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceCCVsHFCC.png"),
           plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
           width = mult*18, height = mult*12, units = "cm")
    
    for(corr in corrs){
      #processedData <- processedData[processedData[, colName] == "None",]
      tempData <- data.frame("CC" = as.numeric(processedData$globalCorrelation_R2),
                             "LCC" = as.numeric(processedData[, paste0("localCorrelationR2_corr", corr)]),
                             "LCC_coeff" = as.numeric(processedData$localCorrelationCoefficientOfVariation_R2),
                             "LCC_sd" = as.numeric(processedData$localCorrelationSD_R2),
                             "perform" = factor(processedData[, colName], levels = refData$algName))
      
      p <- ggplot(tempData, aes(x = LCC, y = CC)) +
            geom_point(aes(color = perform, shape = perform), size = 1) + 
            scale_color_manual(values = setNames(object = refData$col, refData$algName),
                               labels = setNames(object = refData$label, refData$algName),
                               name = "Best algorithm") +
            scale_shape_manual(values = setNames(object = refData$pch, refData$algName),
                               labels = setNames(object = refData$label, refData$algName),
                               name = "Best algorithm") +
            xlab(as.expression(bquote(LCC*textstyle(atop(0.2, .(corr))))))
      ggsave(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceCCVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*18, height = mult*12, units = "cm")
      
      p <- ggplot(tempData, aes(x = LCC, y = LCC_coeff)) +
            geom_point(aes(color = perform, shape = perform), size = 1) + 
            scale_color_manual(values = setNames(object = refData$col, refData$algName),
                               labels = setNames(object = refData$label, refData$algName),
                               name = "Best algorithm") +
            scale_shape_manual(values = setNames(object = refData$pch, refData$algName),
                               labels = setNames(object = refData$label, refData$algName),
                               name = "Best algorithm") +
            xlab(as.expression(bquote(LCC*textstyle(atop(0.2, .(corr)))))) + 
            ylab(as.expression(bquote(LCC*textstyle(atop(0.2, "coeff")))))
      ggsave(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceLCC_coeffVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*18, height = mult*12, units = "cm")
      
      p <- ggplot(tempData, aes(x = LCC, y = LCC_sd)) +
            geom_point(aes(color = perform, shape = perform), size = 1) + 
            scale_color_manual(values = setNames(object = refData$col, refData$algName),
                               labels = setNames(object = refData$label, refData$algName),
                               name = "Best algorithm") +
            scale_shape_manual(values = setNames(object = refData$pch, refData$algName),
                               labels = setNames(object = refData$label, refData$algName),
                               name = "Best algorithm") +
            xlab(as.expression(bquote(LCC*textstyle(atop(0.2, .(corr)))))) + 
            ylab(as.expression(bquote(LCC*textstyle(atop(0.2, "sd")))))
      ggsave(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceLCC_sdVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*18, height = mult*12, units = "cm")
    }
    
    
    
    
    
    
    
    
    
    
    # png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceGlobalCorrVsWeightedLowFiCorr.png"), width = mult*682, height = mult*682)
    # plot(c(), c(),
    #      col = "black",
    #      type = "p",
    #      ylab = "sqrt(CC)",
    #      xlab = "Weighted correlation",
    #      ylim = c(-1.1, 1.1),
    #      xlim = c(-1.1, 1.1),
    #      # xaxt = "n",
    #      pch = 20)
    # 
    # for(row in 1:nrow(refData)){
    #   points(processedData[processedData[, colName] == refData[row, "algName"], "lowFiLowCorrelation"], processedData[processedData[, colName] == refData[row, "algName"], "globalCorrelation"], pch = refData[row, "pch"], col = refData[row, "col"])
    # }
    # lines(c(-1, 1), c(-1, 1), type = "l", lty = "longdash")
    # #lines(c(-1, 0.85), c(-0.85, 1), type = "l", lty = "dotdash")
    # #lines(c(-0.85, 1), c(-1, 0.85), type = "l", lty = "dotdash")
    # #lines(c(-0.90, -0.90), c(-1, 1), type = "l", lty = "dotdash")
    # #lines(c(0.90, 0.90), c(-1, 1), type = "l", lty = "dotdash")
    # lines(c(-0.95, -0.95), c(-1, 1), type = "l", lty = "dotdash")
    # lines(c(0.95, 0.95), c(-1, 1), type = "l", lty = "dotdash")
    # #lines(c(-1, 1), c(-0.90, -0.90), type = "l", lty = "dotdash")
    # #lines(c(-1, 1), c(0.90, 0.90), type = "l", lty = "dotdash")
    # lines(c(-1, 1), c(-0.95, -0.95), type = "l", lty = "dotdash")
    # lines(c(-1, 1), c(0.95, 0.95), type = "l", lty = "dotdash")
    # legend("topleft", legend = refData$label, pch = refData$pch, col = refData$col)
    # out <- dev.off()
    # 
    # 
    # png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceGlobalCorrVsWeightedHighFiCorr.png"), width = mult*682, height = mult*682)
    #     plot(c(), c(),
    #          col = "black",
    #          type = "p",
    #          ylab = "sqrt(CC)",
    #          xlab = "Weighted correlation",
    #          ylim = c(-1.1, 1.1),
    #          xlim = c(-1.1, 1.1),
    #          # xaxt = "n",
    #          pch = 20)
    # for(row in 1:nrow(refData)){
    #   points(processedData[processedData[, colName] == refData[row, "algName"], "highFiLowCorrelation"], processedData[processedData[, colName] == refData[row, "algName"], "globalCorrelation"], pch = refData[row, "pch"], col = refData[row, "col"])
    # }
    # lines(c(-1, 1), c(-1, 1), type = "l", lty = "longdash")
    # #lines(c(-1, 0.85), c(-0.85, 1), type = "l", lty = "dotdash")
    # #lines(c(-0.85, 1), c(-1, 0.85), type = "l", lty = "dotdash")
    # #lines(c(-0.90, -0.90), c(-1, 1), type = "l", lty = "dotdash")
    # #lines(c(0.90, 0.90), c(-1, 1), type = "l", lty = "dotdash")
    # lines(c(-0.95, -0.95), c(-1, 1), type = "l", lty = "dotdash")
    # lines(c(0.95, 0.95), c(-1, 1), type = "l", lty = "dotdash")
    # #lines(c(-1, 1), c(-0.90, -0.90), type = "l", lty = "dotdash")
    # #lines(c(-1, 1), c(0.90, 0.90), type = "l", lty = "dotdash")
    # lines(c(-1, 1), c(-0.95, -0.95), type = "l", lty = "dotdash")
    # lines(c(-1, 1), c(0.95, 0.95), type = "l", lty = "dotdash")
    # legend("topleft", legend = refData$label, pch = refData$pch, col = refData$col)
    # out <- dev.off()
    # 
    # png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceLowFiCorrVsWeightedHighFiCorr.png"), width = mult*682, height = mult*682)
    # plot(c(), c(),
    #      col = "black",
    #      type = "p",
    #      ylab = "Weighted correlation low",
    #      xlab = "Weighted correlation high",
    #      ylim = c(-1.1, 1.1),
    #      xlim = c(-1.1, 1.1),
    #      # xaxt = "n",
    #      pch = 20)
    # for(row in 1:nrow(refData)){
    #   points(processedData[processedData[, colName] == refData[row, "algName"], "highFiLowCorrelation"], processedData[processedData[, colName] == refData[row, "algName"], "lowFiLowCorrelation"], pch = refData[row, "pch"], col = refData[row, "col"])
    # }
    # lines(c(-1, 1), c(-1, 1), type = "l", lty = "longdash")
    # #lines(c(-1, 0.85), c(-0.85, 1), type = "l", lty = "dotdash")
    # #lines(c(-0.85, 1), c(-1, 0.85), type = "l", lty = "dotdash")
    # #lines(c(-0.90, -0.90), c(-1, 1), type = "l", lty = "dotdash")
    # #lines(c(0.90, 0.90), c(-1, 1), type = "l", lty = "dotdash")
    # lines(c(-0.95, -0.95), c(-1, 1), type = "l", lty = "dotdash")
    # lines(c(0.95, 0.95), c(-1, 1), type = "l", lty = "dotdash")
    # #lines(c(-1, 1), c(-0.90, -0.90), type = "l", lty = "dotdash")
    # #lines(c(-1, 1), c(0.90, 0.90), type = "l", lty = "dotdash")
    # lines(c(-1, 1), c(-0.95, -0.95), type = "l", lty = "dotdash")
    # lines(c(-1, 1), c(0.95, 0.95), type = "l", lty = "dotdash")
    # legend("topleft", legend = refData$label, pch = refData$pch, col = refData$col)
    # out <- dev.off() 
    #   
    # 
    # png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceGlobalCorrVsLocalError.png"), width = mult*682, height = mult*682)
    # plot(c(), c(),
    #      col = "black",
    #      type = "p",
    #      ylab = "sqrt(CC)",
    #      xlab = "RRMSE",
    #      ylim = c(-1.1, 1.1),
    #      #xlim = c(-0.1, 1.1 * max(as.numeric(processedData$relativeError))),
    #      xlim = c(-0.1, 3),
    #      # xaxt = "n",
    #      pch = 20)
    # for(row in 1:nrow(refData)){
    #   points(processedData[processedData[, colName] == refData[row, "algName"], "relativeError"], processedData[processedData[, colName] == refData[row, "algName"], "globalCorrelation"], pch = refData[row, "pch"], col = refData[row, "col"])
    # }
    # legend("topright", legend = refData$label, pch = refData$pch, col = refData$col)
    # out <- dev.off()
    # 
    # png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceGlobalCorrVsLocalErrorZoom.png"), width = mult*682, height = mult*682)
    # plot(c(), c(),
    #      col = "black",
    #      type = "p",
    #      ylab = "sqrt(CC)",
    #      xlab = "RMSE",
    #      ylim = c(0.5, 1.05),
    #      xlim = c(-0.1, 1),
    #      # xaxt = "n",
    #      pch = 20)
    # for(row in 1:nrow(refData)){
    #   points(processedData[processedData[, colName] == refData[row, "algName"], "relativeError"], processedData[processedData[, colName] == refData[row, "algName"], "globalCorrelation"], pch = refData[row, "pch"], col = refData[row, "col"])
    # }
    # legend("topright", legend = refData$label, pch = refData$pch, col = refData$col)
    # out <- dev.off()
    # 
    # 
    # 
    # 
    # for(corr in corrs){  
    #   png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceGlobalCorrVsLocalCorrAbs-", corr, ".png"), width = mult*682, height = mult*682)
    #   plot(c(), c(),
    #        col = "black",
    #        type = "p",
    #        ylab = "sqrt(CC)",
    #        xlab = paste0("Probability abs(local correlation) > ", corr),
    #        ylim = c(-1.1, 1.1),
    #        xlim = c(-0.1, 1.1),
    #        # xaxt = "n",
    #        pch = 20)
    #   for(row in 1:nrow(refData)){
    #     points(processedData[processedData[, colName] == refData[row, "algName"], paste0("localCorrelationAbs_corr", corr)], processedData[processedData[, colName] == refData[row, "algName"], "globalCorrelation"], pch = refData[row, "pch"], col = refData[row, "col"])
    #   }
    #   lines(c(-0.1, 1.1), c(corr, corr), type = "l", lty = "longdash")
    #   legend("bottomright", legend = refData$label, pch = refData$pch, col = refData$col)
    #   out <- dev.off()
    # 
    #   
    #   png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceLocalCorrelationCoefficientOfVariationVsLocalCorrAbs-", corr, ".png"), width = mult*682, height = mult*682)
    #   plot(c(), c(),
    #        col = "black",
    #        type = "p",
    #        ylab = "Local correlation Coefficient of Variation",
    #        xlab = paste0("Probability abs(local correlation) > ", corr),
    #        ylim = c(-0.1, 2.5),
    #        xlim = c(-0.1, 1.1),
    #        # xaxt = "n",
    #        pch = 20)
    #   for(row in 1:nrow(refData)){
    #     points(processedData[processedData[, colName] == refData[row, "algName"], paste0("localCorrelationAbs_corr", corr)], processedData[processedData[, colName] == refData[row, "algName"], "localCorrelationCoefficientOfVariation"], pch = refData[row, "pch"], col = refData[row, "col"])
    #   }
    #   legend("topleft", legend = refData$label, pch = refData$pch, col = refData$col)
    #   out <- dev.off()
    #   
    #   png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceLocalCorrelationSDVsLocalCorrAbs-", corr, ".png"), width = mult*682, height = mult*682)
    #   plot(c(), c(),
    #        col = "black",
    #        type = "p",
    #        ylab = "Local correlation SD",
    #        xlab = paste0("Probability abs(local correlation) > ", corr),
    #        ylim = c(-0.1, 1.1),
    #        xlim = c(-0.1, 1.1),
    #        # xaxt = "n",
    #        pch = 20)
    #   for(row in 1:nrow(refData)){
    #     points(processedData[processedData[, colName] == refData[row, "algName"], paste0("localCorrelationAbs_corr", corr)], processedData[processedData[, colName] == refData[row, "algName"], "localCorrelationSD"], pch = refData[row, "pch"], col = refData[row, "col"])
    #   }
    #   legend("topleft", legend = refData$label, pch = refData$pch, col = refData$col)
    #   out <- dev.off()
    # }
    
    
    
    
    
    

    # Here the same things but now using r2 instead
  #   png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-r2_PerformanceGlobalCorrVsWeightedLowFiCorr.png"), width = mult*682, height = mult*682)
  #   plot(c(), c(),
  #        col = "black",
  #        type = "p",
  #        #ylab = as.expression(bquote(CC^2)),
  #        ylab = "CC",
  #        xlab = "Weighted correlation (r2)",
  #        ylim = c(-0.1, 1.1),
  #        xlim = c(-0.1, 1.1),
  #        # xaxt = "n",
  #        pch = 20)
  #   
  #   for(row in 1:nrow(refData)){
  #     points(processedData[processedData[, colName] == refData[row, "algName"], "lowFiLowCorrelation_R2"], processedData[processedData[, colName] == refData[row, "algName"], "globalCorrelation_R2"], pch = refData[row, "pch"], col = refData[row, "col"])
  #   }
  #   legend("topleft", legend = refData$label, pch = refData$pch, col = refData$col)
  #   lines(c(0, 1), c(0, 1), type = "l", lty = "longdash")
  #   #lines(c(0, 0.85), c(0.15, 1), type = "l", lty = "dotdash")
  #   #lines(c(0.15, 1), c(0, 0.85), type = "l", lty = "dotdash")
  #   #lines(c(0.1, 0.1), c(0, 1), type = "l", lty = "dotdash")
  #   lines(c(0.90, 0.90), c(0, 1), type = "l", lty = "dotdash")
  #   #lines(c(0.05, 0.05), c(0, 1), type = "l", lty = "dotdash")
  #   #lines(c(0.95, 0.95), c(0, 1), type = "l", lty = "dotdash")
  #   #lines(c(0, 1), c(0.1, 0.1), type = "l", lty = "dotdash")
  #   lines(c(0, 1), c(0.90, 0.90), type = "l", lty = "dotdash")
  #   #lines(c(0, 1), c(0.05, 0.05), type = "l", lty = "dotdash")
  #   #lines(c(0, 1), c(0.95, 0.95), type = "l", lty = "dotdash")
  #   out <- dev.off()
  #   
  #   
  #   png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-r2_PerformanceGlobalCorrVsWeightedHighFiCorr.png"), width = mult*682, height = mult*682)
  #   plot(c(), c(),
  #        col = "black",
  #        type = "p",
  #        #ylab = as.expression(bquote(CC^2)),
  #        ylab = "CC",
  #        xlab = "Weighted correlation (r2)",
  #        ylim = c(-0.1, 1.1),
  #        xlim = c(-0.1, 1.1),
  #        # xaxt = "n",
  #        pch = 20)
  #   for(row in 1:nrow(refData)){
  #     points(processedData[processedData[, colName] == refData[row, "algName"], "highFiLowCorrelation_R2"], processedData[processedData[, colName] == refData[row, "algName"], "globalCorrelation_R2"], pch = refData[row, "pch"], col = refData[row, "col"])
  #   }
  #   lines(c(0, 1), c(0, 1), type = "l", lty = "longdash")
  #   #lines(c(0, 0.85), c(0.15, 1), type = "l", lty = "dotdash")
  #   #lines(c(0.15, 1), c(0, 0.85), type = "l", lty = "dotdash")
  #   #lines(c(0.1, 0.1), c(0, 1), type = "l", lty = "dotdash")
  #   lines(c(0.90, 0.90), c(0, 1), type = "l", lty = "dotdash")
  #   #lines(c(0.05, 0.05), c(0, 1), type = "l", lty = "dotdash")
  #   #lines(c(0.95, 0.95), c(0, 1), type = "l", lty = "dotdash")
  #   #lines(c(0, 1), c(0.1, 0.1), type = "l", lty = "dotdash")
  #   lines(c(0, 1), c(0.90, 0.90), type = "l", lty = "dotdash")
  #   #lines(c(0, 1), c(0.05, 0.05), type = "l", lty = "dotdash")
  #   #lines(c(0, 1), c(0.95, 0.95), type = "l", lty = "dotdash")
  #   legend("topleft", legend = refData$label, pch = refData$pch, col = refData$col)
  #   out <- dev.off()
  #   
  #   png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-r2_PerformanceLowFiCorrVsWeightedHighFiCorr.png"), width = mult*682, height = mult*682)
  #   plot(c(), c(),
  #        col = "black",
  #        type = "p",
  #        ylab = "Weighted correlation low (r2)",
  #        xlab = "Weighted correlation high (r2)",
  #        ylim = c(-0.1, 1.1),
  #        xlim = c(-0.1, 1.1),
  #        # xaxt = "n",
  #        pch = 20)
  #   for(row in 1:nrow(refData)){
  #     points(processedData[processedData[, colName] == refData[row, "algName"], "highFiLowCorrelation_R2"], processedData[processedData[, colName] == refData[row, "algName"], "lowFiLowCorrelation_R2"], pch = refData[row, "pch"], col = refData[row, "col"])
  #   }
  #   lines(c(0, 1), c(0, 1), type = "l", lty = "longdash")
  #   #lines(c(0, 0.85), c(0.15, 1), type = "l", lty = "dotdash")
  #   #lines(c(0.15, 1), c(0, 0.85), type = "l", lty = "dotdash")
  #   #lines(c(0.1, 0.1), c(0, 1), type = "l", lty = "dotdash")
  #   lines(c(0.90, 0.90), c(0, 1), type = "l", lty = "dotdash")
  #   #lines(c(0.05, 0.05), c(0, 1), type = "l", lty = "dotdash")
  #   #lines(c(0.95, 0.95), c(0, 1), type = "l", lty = "dotdash")
  #   #lines(c(0, 1), c(0.1, 0.1), type = "l", lty = "dotdash")
  #   lines(c(0, 1), c(0.90, 0.90), type = "l", lty = "dotdash")
  #   #lines(c(0, 1), c(0.05, 0.05), type = "l", lty = "dotdash")
  #   #lines(c(0, 1), c(0.95, 0.95), type = "l", lty = "dotdash")
  #   legend("topleft", legend = refData$label, pch = refData$pch, col = refData$col)
  #   out <- dev.off() 
  #   
  #   
  #   png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-r2_PerformanceGlobalCorrVsLocalError.png"), width = mult*682, height = mult*682)
  #   plot(c(), c(),
  #        col = "black",
  #        type = "p",
  #        #ylab = as.expression(bquote(CC^2)),
  #        ylab = "CC",
  #        xlab = "RRMSE",
  #        ylim = c(-0.1, 1.1),
  #        #xlim = c(-0.1, 1.1 * max(as.numeric(processedData$relativeError))),
  #        xlim = c(-0.1, 3),
  #        # xaxt = "n",
  #        pch = 20)
  #   for(row in 1:nrow(refData)){
  #     points(processedData[processedData[, colName] == refData[row, "algName"], "relativeError"], processedData[processedData[, colName] == refData[row, "algName"], "globalCorrelation_R2"], pch = refData[row, "pch"], col = refData[row, "col"])
  #   }
  #   legend("topright", legend = refData$label, pch = refData$pch, col = refData$col)
  #   out <- dev.off()
  #   
  #   png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-r2_PerformanceGlobalCorrVsLocalErrorZoom.png"), width = mult*682, height = mult*682)
  #   plot(c(), c(),
  #        col = "black",
  #        type = "p",
  #        ylab = as.expression(bquote(CC^2)),
  #        xlab = "RRMSE",
  #        ylim = c(-0.1, 1.05),
  #        xlim = c(-0.1, 1),
  #        # xaxt = "n",
  #        pch = 20)
  #   for(row in 1:nrow(refData)){
  #     points(processedData[processedData[, colName] == refData[row, "algName"], "relativeError"], processedData[processedData[, colName] == refData[row, "algName"], "globalCorrelation_R2"], pch = refData[row, "pch"], col = refData[row, "col"])
  #   }
  #   legend("topright", legend = refData$label, pch = refData$pch, col = refData$col)
  #   out <- dev.off()
  #   
  #   
  #   
  #   
  #   for(corr in corrs){
  #   
  #     
  #     
  #     
  #     png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-r2_PerformanceGlobalCorrVsLocalCorrAbs-", corr, ".png"), width = mult*682, height = mult*682)
  #     plot(c(), c(),
  #          col = "black",
  #          type = "p",
  #          #ylab = as.expression(bquote(CC^2)),
  #          ylab = "CC",
  #          #xlab = paste0("Probability (local correlation)^2 > ", corr),
  #          xlab = as.expression(bquote(LCC*textstyle(atop(0.2, .(corr))))),
  #          ylim = c(-0.1, 1.1),
  #          xlim = c(-0.1, 1.1),
  #          # xaxt = "n",
  #          pch = 20)
  #     for(row in 1:nrow(refData)){
  #       points(processedData[processedData[, colName] == refData[row, "algName"], paste0("localCorrelationR2_corr", corr)], processedData[processedData[, colName] == refData[row, "algName"], "globalCorrelation_R2"], pch = refData[row, "pch"], col = refData[row, "col"])
  #     }
  #     lines(c(-0.1, 1.1), c(corr, corr), type = "l", lty = "longdash")
  #     legend("bottomright", legend = refData$label, pch = refData$pch, col = refData$col)
  #     out <- dev.off()
  #     
  #     
  #     png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-r2_PerformanceLocalCorrelationCoefficientOfVariationVsLocalCorrAbs-", corr, ".png"), width = mult*682, height = mult*682)
  #     plot(c(), c(),
  #          col = "black",
  #          type = "p",
  #          #ylab = "Local correlation Coefficient of Variation (r2)",
  #          ylab = as.expression(bquote(LCC*textstyle(atop(0.2, "coeff")))),
  #          #xlab = paste0("Probability (local correlation)^2 > ", corr),
  #          xlab = as.expression(bquote(LCC*textstyle(atop(0.2, .(corr))))),
  #          ylim = c(-0.1, 2.5),
  #          xlim = c(-0.1, 1.1),
  #          # xaxt = "n",
  #          pch = 20,
  #          mgp=c(2.5,1,0))
  #     for(row in 1:nrow(refData)){
  #       points(processedData[processedData[, colName] == refData[row, "algName"], paste0("localCorrelationR2_corr", corr)], processedData[processedData[, colName] == refData[row, "algName"], "localCorrelationCoefficientOfVariation_R2"], pch = refData[row, "pch"], col = refData[row, "col"])
  #     }
  #     legend("topleft", legend = refData$label, pch = refData$pch, col = refData$col)
  #     par(mar=c(5, 4, 4, 2) + 0.1)
  #     out <- dev.off()
  #     
  #     png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-r2_PerformanceLocalCorrelationSDVsLocalCorrAbs-", corr, ".png"), width = mult*682, height = mult*682)
  #     plot(c(), c(),
  #          col = "black",
  #          type = "p",
  #          #ylab = "Local correlation SD (r2)",
  #          ylab = as.expression(bquote(LCC*textstyle(atop(0.2, "sd")))),
  #          #xlab = paste0("Probability (local correlation)^2 > ", corr),
  #          xlab = as.expression(bquote(LCC*textstyle(atop(0.2, .(corr))))),
  #          ylim = c(-0.1, 1.1),
  #          xlim = c(-0.1, 1.1),
  #          # xaxt = "n",
  #          pch = 20,
  #          mgp=c(2.5,1,0))
  #     for(row in 1:nrow(refData)){
  #       points(processedData[processedData[, colName] == refData[row, "algName"], paste0("localCorrelationR2_corr", corr)], processedData[processedData[, colName] == refData[row, "algName"], "localCorrelationSD_R2"], pch = refData[row, "pch"], col = refData[row, "col"])
  #     }
  #     legend("topleft", legend = refData$label, pch = refData$pch, col = refData$col)
  #     out <- dev.off()
  #   }
  # 
  # 
  #   
  #   
  #   # png(paste0("../data/analysis/modelPerformance/", dataName, "-", performanceMeasurePrefix, "-PerformanceGlobalCorrVsLocalCoeffOfVariation.png"), width = 682, height = 682)
  #   # plot(c(), c(),
  #   #      col = "black",
  #   #      type = "p",
  #   #      ylab = "Global correlation",
  #   #      xlab = "Local correlation SD",
  #   #      ylim = c(-1.1, 1.1),
  #   #      xlim = c(-0.1, 1.1),
  #   #      # xaxt = "n",
  #   #      pch = 20)
  #   # for(row in 1:nrow(refData)){
  #   #   points(processedData[processedData[, colName] == refData[row, "algName"], "coefficientOfVariation"], processedData[processedData[, colName] == refData[row, "algName"], "globalCorrelation"], pch = refData[row, "pch"], col = refData[row, "col"])
  #   # }
  #   # legend("topright", legend = refData$label, pch = refData$pch, col = refData$col)
  #   # out <- dev.off()
  #   
  }
}

#dataCombined <- combineArrayResults("modelPerformanceSetup", 1, 832)
#dataCombined$instance <- as.character(dataCombined$instance)
#data <- getAveragePerformance("modelPerformanceSetup", 1, 832)
#currentData <- assessModelPerformance(data, "literatureAnalysis", c(0.85, 0.9, 0.95, 0.975))
#currentData <- plotInstanceFeatures(data, "literatureAnalysis", c(0.85, 0.9, 0.95, 0.975))



