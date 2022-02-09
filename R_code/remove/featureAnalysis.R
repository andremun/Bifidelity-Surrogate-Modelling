library("stringr")
library(egg)
library(grid)



# Again want to make a function out of this
plotInstanceFeatures <- function(processedData, dataName, corrs, mult = 1.0, legend = TRUE){
  pointSize <- 1
  smallDim <- 10
  # First check if data comes with a specification on pch and colour
  if(!("pch" %in% colnames(processedData))){processedData$pch = 1}
  if(!("col" %in% colnames(processedData))){processedData$col = "black"}
  if(!("legend" %in% colnames(processedData))){processedData$legend = ""}
  
  tempData <- data.frame("CC" = as.numeric(processedData$globalCorrelation_R2),
                         "RRMSE" = as.numeric(processedData$relativeError),
                         "HFCC" = as.numeric(processedData$highFiLowCorrelation_R2),
                         "pch" = processedData$pch,
                         "col" = as.character(processedData$col),
                         "legend" = factor(processedData$legend, levels = c("Fixed", "Parameter-based", "Error-based", "Disturbance-based")))

  p <- ggplot(tempData, aes(x = RRMSE, y = CC)) +
        geom_point(aes(shape = legend, color = legend), size = pointSize) + 
        scale_color_manual(values = setNames(object = as.character(unique(tempData$col)), unique(tempData$legend)),
                            labels = setNames(object = as.character(unique(tempData$legend)), unique(tempData$legend)),
                            name = "Instance type") +
        scale_shape_manual(values = setNames(object = unique(tempData$pch), unique(tempData$legend)),
                            labels = setNames(object = as.character(unique(tempData$legend)), unique(tempData$legend)),
                            name = "Instance type")
  if(legend){
    ggsave(paste0("../data/analysis/features/", dataName, "-ComparisonCCVsRRMSE.png"),
           plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
           width = mult*18, height = mult*12, units = "cm")
  }else{
    p <- p + theme(legend.position="none")
    ggsave(paste0("../data/analysis/features/", dataName, "-ComparisonCCVsRRMSE.png"),
           plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
           width = mult*12, height = mult*12, units = "cm")
  }
  
  
  p <- ggplot(tempData, aes(x = HFCC, y = CC)) +
        geom_point(aes(shape = legend, color = legend), size = pointSize) + 
        scale_color_manual(values = setNames(object = as.character(unique(tempData$col)), unique(tempData$legend)),
                           labels = setNames(object = as.character(unique(tempData$legend)), unique(tempData$legend)),
                           name = "Instance type") +
        scale_shape_manual(values = setNames(object = unique(tempData$pch), unique(tempData$legend)),
                           labels = setNames(object = as.character(unique(tempData$legend)), unique(tempData$legend)),
                           name = "Instance type")
  if(legend){
    ggsave(paste0("../data/analysis/features/", dataName, "-ComparisonCCVsHFCC.png"),
         plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
         width = mult*18, height = mult*12, units = "cm")
  }else{
    p <- p + theme(legend.position="none")
    ggsave(paste0("../data/analysis/features/", dataName, "-ComparisonCCVsHFCC.png"),
           plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
           width = mult*12, height = mult*12, units = "cm")
  }
    
  for(corr in corrs){
    #processedData <- processedData[processedData[, colName] == "None",]
    tempData <- data.frame("CC" = as.numeric(processedData$globalCorrelation_R2),
                           "LCC" = as.numeric(processedData[, paste0("localCorrelationR2_corr", corr)]),
                           "LCC_coeff" = as.numeric(processedData$localCorrelationCoefficientOfVariation_R2),
                           "LCC_sd" = as.numeric(processedData$localCorrelationSD_R2),
                           "pch" = processedData$pch,
                           "col" = as.character(processedData$col),
                           "legend" = factor(processedData$legend, levels = c("Fixed", "Parameter-based", "Error-based", "Disturbance-based")))
    
    p <- ggplot(tempData, aes(x = LCC, y = CC)) +
          geom_point(aes(shape = legend, color = legend), size = pointSize) + 
          scale_color_manual(values = setNames(object = as.character(unique(tempData$col)), unique(tempData$legend)),
                             labels = setNames(object = as.character(unique(tempData$legend)), unique(tempData$legend)),
                             name = "Instance type") +
          scale_shape_manual(values = setNames(object = unique(tempData$pch), unique(tempData$legend)),
                             labels = setNames(object = as.character(unique(tempData$legend)), unique(tempData$legend)),
                             name = "Instance type") +
          xlab(as.expression(bquote(LCC*textstyle(atop(0.2, .(corr))))))
    if(legend){
      ggsave(paste0("../data/analysis/features/", dataName, "-ComparisonCCVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*18, height = mult*12, units = "cm")
    }else{
      p <- p + theme(legend.position="none")
      ggsave(paste0("../data/analysis/features/", dataName, "-ComparisonCCVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*12, height = mult*12, units = "cm")
    }
    
    p <- ggplot(tempData, aes(x = LCC, y = LCC_coeff)) +
          geom_point(aes(shape = legend, color = legend), size = pointSize) + 
          scale_color_manual(values = setNames(object = as.character(unique(tempData$col)), unique(tempData$legend)),
                             labels = setNames(object = as.character(unique(tempData$legend)), unique(tempData$legend)),
                             name = "Instance type") +
          scale_shape_manual(values = setNames(object = unique(tempData$pch), unique(tempData$legend)),
                             labels = setNames(object = as.character(unique(tempData$legend)), unique(tempData$legend)),
                             name = "Instance type") +
          xlab(as.expression(bquote(LCC*textstyle(atop(0.2, .(corr)))))) + 
          ylab(as.expression(bquote(LCC*textstyle(atop(0.2, "coeff")))))
    
    if(legend){
      ggsave(paste0("../data/analysis/features/", dataName, "-ComparisonLCC_coeffVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*18, height = mult*12, units = "cm")
    }else{
      p <- p + theme(legend.position="none")
      ggsave(paste0("../data/analysis/features/", dataName, "-ComparisonLCC_coeffVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*12, height = mult*12, units = "cm")
    }
    
    p <- ggplot(tempData, aes(x = LCC, y = LCC_sd)) +
          geom_point(aes(shape = legend, color = legend), size = pointSize) + 
          scale_color_manual(values = setNames(object = as.character(unique(tempData$col)), unique(tempData$legend)),
                             labels = setNames(object = as.character(unique(tempData$legend)), unique(tempData$legend)),
                             name = "Instance type") +
          scale_shape_manual(values = setNames(object = unique(tempData$pch), unique(tempData$legend)),
                             labels = setNames(object = as.character(unique(tempData$legend)), unique(tempData$legend)),
                             name = "Instance type") +
          xlab(as.expression(bquote(LCC*textstyle(atop(0.2, .(corr)))))) + 
          ylab(as.expression(bquote(LCC*textstyle(atop(0.2, "sd")))))
    if(legend){
      ggsave(paste0("../data/analysis/features/", dataName, "-ComparisonLCC_sdVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*18, height = mult*12, units = "cm")
    }else{
      p <- p + theme(legend.position="none")
      ggsave(paste0("../data/analysis/features/", dataName, "-ComparisonLCC_sdVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*12, height = mult*12, units = "cm")
    }
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  # # First feature analysis
  # png(paste0("../data/analysis/features/", dataName, "-ComparisonGlobalCorrVsWeightedLowFiCorr.png"), width = mult*682, height = mult*682)
  # plot(c(), c(),
  #      col = "black",
  #      type = "p",
  #      ylab = "sqrt(CC)",
  #      xlab = "Weighted correlation",
  #      ylim = c(-1.1, 1.1),
  #      xlim = c(-1.1, 1.1),
  #      # xaxt = "n",
  #      pch = 20)
  # pchs <- c()
  # cols <- c()
  # legends <- c()
  # for(pch in unique(processedData$pch)){
  #   for(col in unique(processedData[processedData$pch == pch, "col"])){
  #     for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #       points(processedData[processedData$pch == pch & processedData$col == col, "lowFiLowCorrelation"], processedData[processedData$pch == pch & processedData$col == col, "globalCorrelation"], pch = pch, col = col)
  #       pchs <- c(pchs, pch)
  #       cols <- c(cols, col)
  #       if(legend != ""){legends <- c(legends, legend)}
  #     }
  #   }
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
  # if(length(legends) >= 1){legend("topleft", legend = legends, pch = pchs, col = cols)}
  # out <- dev.off()
  # 
  # 
  # 
  # png(paste0("../data/analysis/features/", dataName, "-ComparisonGlobalCorrVsWeightedHighFiCorr.png"), width = mult*682, height = mult*682)
  # plot(c(), c(),
  #      col = "black",
  #      type = "p",
  #      ylab = "sqrt(CC)",
  #      xlab = "Weighted correlation",
  #      ylim = c(-1.1, 1.1),
  #      xlim = c(-1.1, 1.1),
  #      # xaxt = "n",
  #      pch = 20)
  # pchs <- c()
  # cols <- c()
  # legends <- c()
  # for(pch in unique(processedData$pch)){
  #   for(col in unique(processedData[processedData$pch == pch, "col"])){
  #     for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #       points(processedData[processedData$pch == pch & processedData$col == col, "highFiLowCorrelation"], processedData[processedData$pch == pch & processedData$col == col, "globalCorrelation"], pch = pch, col = col)
  #       pchs <- c(pchs, pch)
  #       cols <- c(cols, col)
  #       if(legend != ""){legends <- c(legends, legend)}
  #     }
  #   }
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
  # if(length(legends) >= 1){legend("topleft", legend = legends, pch = pchs, col = cols)}
  # out <- dev.off()
  # 
  # png(paste0("../data/analysis/features/", dataName, "-ComparisonLowFiCorrVsWeightedHighFiCorr.png"), width = mult*682, height = mult*682)
  # plot(c(), c(),
  #      col = "black",
  #      type = "p",
  #      ylab = "Weighted correlation low",
  #      xlab = "Weighted correlation high",
  #      ylim = c(-1.1, 1.1),
  #      xlim = c(-1.1, 1.1),
  #      # xaxt = "n",
  #      pch = 20)
  # pchs <- c()
  # cols <- c()
  # legends <- c()
  # for(pch in unique(processedData$pch)){
  #   for(col in unique(processedData[processedData$pch == pch, "col"])){
  #     for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #       points(processedData[processedData$pch == pch & processedData$col == col, "highFiLowCorrelation"], processedData[processedData$pch == pch & processedData$col == col, "lowFiLowCorrelation"], pch = pch, col = col)
  #       pchs <- c(pchs, pch)
  #       cols <- c(cols, col)
  #       if(legend != ""){legends <- c(legends, legend)}
  #     }
  #   }
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
  # if(length(legends) >= 1){legend("topleft", legend = legends, pch = pchs, col = cols)}
  # out <- dev.off()
  # 
  # png(paste0("../data/analysis/features/", dataName, "-ComparisonGlobalCorrVsRelativeError.png"), width = mult*682, height = mult*682)
  # plot(c(), c(),
  #      col = "black",
  #      type = "p",
  #      ylab = "sqrt(CC)",
  #      xlab = "RRMSE",
  #      ylim = c(-1.1, 1.1),
  #      xlim = c(-0.1, 1.1 * max(as.numeric(processedData$relativeError))),
  #      # xaxt = "n",
  #      pch = 20)
  # pchs <- c()
  # cols <- c()
  # legends <- c()
  # for(pch in unique(processedData$pch)){
  #   for(col in unique(processedData[processedData$pch == pch, "col"])){
  #     for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #       points(processedData[processedData$pch == pch & processedData$col == col, "relativeError"], processedData[processedData$pch == pch & processedData$col == col, "globalCorrelation"], pch = pch, col = col)
  #       pchs <- c(pchs, pch)
  #       cols <- c(cols, col)
  #       if(legend != ""){legends <- c(legends, legend)}
  #     }
  #   }
  # }
  # if(length(legends) >= 1){legend("topright", legend = legends, pch = pchs, col = cols)}
  # out <- dev.off()
  # 
  # for(corr in corrs){
  #   png(paste0("../data/analysis/features/", dataName, "-ComparisonGlobalCorrVsLocalCorrAbs-", corr, ".png"), width = mult*682, height = mult*682)
  #   plot(c(), c(),
  #        col = "black",
  #        type = "p",
  #        ylab = "sqrt(CC)",
  #        xlab = paste0("Probability abs(local correlation) > ", corr),
  #        ylim = c(-1.1, 1.1),
  #        xlim = c(-0.1, 1.1),
  #        # xaxt = "n",
  #        pch = 20)
  #   pchs <- c()
  #   cols <- c()
  #   legends <- c()
  #   for(pch in unique(processedData$pch)){
  #     for(col in unique(processedData[processedData$pch == pch, "col"])){
  #       for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #         points(processedData[processedData$pch == pch & processedData$col == col, paste0("localCorrelationAbs_corr", corr)], processedData[processedData$pch == pch & processedData$col == col, "globalCorrelation"], pch = pch, col = col)
  #         pchs <- c(pchs, pch)
  #         cols <- c(cols, col)
  #         if(legend != ""){legends <- c(legends, legend)}
  #       }
  #     }
  #   }
  #   lines(c(-0.1, 1.1), c(corr, corr), type = "l", lty = "longdash")
  #   if(length(legends) >= 1){legend("bottomright", legend = legends, pch = pchs, col = cols)}
  #   out <- dev.off()
  # 
  #   png(paste0("../data/analysis/features/", dataName, "-ComparisonLocalCoeffOfVariationVsLocalCorrAbs-", corr, ".png"), width = mult*682, height = mult*682)
  #   plot(c(), c(),
  #        col = "black",
  #        type = "p",
  #        ylab = "Local correlation coefficient of variation",
  #        xlab = paste0("Probability abs(local correlation) > ", corr),
  #        ylim = c(-0.1, 2.5),
  #        xlim = c(-0.1, 1.1),
  #        # xaxt = "n",
  #        pch = 20)
  #   pchs <- c()
  #   cols <- c()
  #   legends <- c()
  #   for(pch in unique(processedData$pch)){
  #     for(col in unique(processedData[processedData$pch == pch, "col"])){
  #       for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #         points(processedData[processedData$pch == pch & processedData$col == col, paste0("localCorrelationAbs_corr", corr)], processedData[processedData$pch == pch & processedData$col == col, "localCorrelationCoefficientOfVariation"], pch = pch, col = col)
  #         pchs <- c(pchs, pch)
  #         cols <- c(cols, col)
  #         if(legend != ""){legends <- c(legends, legend)}
  #       }
  #     }
  #   }
  #   if(length(legends) >= 1){legend("topright", legend = legends, pch = pchs, col = cols)}
  #   out <- dev.off()
  # 
  #   png(paste0("../data/analysis/features/", dataName, "-ComparisonLocalVariationSDVsLocalCorrAbs-", corr, ".png"), width = mult*682, height = mult*682)
  #   plot(c(), c(),
  #        col = "black",
  #        type = "p",
  #        ylab = "Local correlation SD",
  #        xlab = paste0("Probability abs(local correlation) > ", corr),
  #        ylim = c(-0.1, 1.1),
  #        xlim = c(-0.1, 1.1),
  #        # xaxt = "n",
  #        pch = 20)
  #   pchs <- c()
  #   cols <- c()
  #   legends <- c()
  #   for(pch in unique(processedData$pch)){
  #     for(col in unique(processedData[processedData$pch == pch, "col"])){
  #       for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #         points(processedData[processedData$pch == pch & processedData$col == col, paste0("localCorrelationAbs_corr", corr)], processedData[processedData$pch == pch & processedData$col == col, "localCorrelationSD"], pch = pch, col = col)
  #         pchs <- c(pchs, pch)
  #         cols <- c(cols, col)
  #         if(legend != ""){legends <- c(legends, legend)}
  #       }
  #     }
  #   }
  #   if(length(legends) >= 1){legend("topright", legend = legends, pch = pchs, col = cols)}
  #   out <- dev.off()
  # }
  # 
  # 
  
  
  
  
  
  # Here the same things but now using r2 instead
  # png(paste0("../data/analysis/features/", dataName, "-r2_ComparisonGlobalCorrVsWeightedLowFiCorr.png"), width = mult*682, height = mult*682)
  # plot(c(), c(),
  #      col = "black",
  #      type = "p",
  #      #ylab = as.expression(bquote(CC^2)),
  #      ylab = "CC",
  #      xlab = "Weighted correlation (r2)",
  #      ylim = c(-0.1, 1.1),
  #      xlim = c(-0.1, 1.1),
  #      # xaxt = "n",
  #      pch = 20)
  # pchs <- c()
  # cols <- c()
  # legends <- c()
  # for(pch in unique(processedData$pch)){
  #   for(col in unique(processedData[processedData$pch == pch, "col"])){
  #     for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #       points(processedData[processedData$pch == pch & processedData$col == col, "lowFiLowCorrelation_R2"], processedData[processedData$pch == pch & processedData$col == col, "globalCorrelation_R2"], pch = pch, col = col)
  #       pchs <- c(pchs, pch)
  #       cols <- c(cols, col)
  #       if(legend != ""){legends <- c(legends, legend)}
  #     }
  #   }
  # }
  # lines(c(0, 1), c(0, 1), type = "l", lty = "longdash")
  # #lines(c(0, 0.85), c(0.15, 1), type = "l", lty = "dotdash")
  # #lines(c(0.15, 1), c(0, 0.85), type = "l", lty = "dotdash")
  # #lines(c(0.1, 0.1), c(0, 1), type = "l", lty = "dotdash")
  # lines(c(0.90, 0.90), c(0, 1), type = "l", lty = "dotdash")
  # #lines(c(0.05, 0.05), c(0, 1), type = "l", lty = "dotdash")
  # #lines(c(0.95, 0.95), c(0, 1), type = "l", lty = "dotdash")
  # #lines(c(0, 1), c(0.1, 0.1), type = "l", lty = "dotdash")
  # lines(c(0, 1), c(0.90, 0.90), type = "l", lty = "dotdash")
  # #lines(c(0, 1), c(0.05, 0.05), type = "l", lty = "dotdash")
  # #lines(c(0, 1), c(0.95, 0.95), type = "l", lty = "dotdash")
  # if(length(legends) >= 1){legend("topleft", legend = legends, pch = pchs, col = cols)}
  # out <- dev.off()
  # 
  # 
  # 
  # png(paste0("../data/analysis/features/", dataName, "-r2_ComparisonGlobalCorrVsWeightedHighFiCorr.png"), width = mult*682, height = mult*682)
  # plot(c(), c(),
  #      col = "black",
  #      type = "p",
  #      #ylab = as.expression(bquote(CC^2)),
  #      ylab = "CC",
  #      xlab = "Weighted correlation (r2)",
  #      ylim = c(-0.1, 1.1),
  #      xlim = c(-0.1, 1.1),
  #      # xaxt = "n",
  #      pch = 20)
  # pchs <- c()
  # cols <- c()
  # legends <- c()
  # for(pch in unique(processedData$pch)){
  #   for(col in unique(processedData[processedData$pch == pch, "col"])){
  #     for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #       points(processedData[processedData$pch == pch & processedData$col == col, "highFiLowCorrelation_R2"], processedData[processedData$pch == pch & processedData$col == col, "globalCorrelation_R2"], pch = pch, col = col)
  #       pchs <- c(pchs, pch)
  #       cols <- c(cols, col)
  #       if(legend != ""){legends <- c(legends, legend)}
  #     }
  #   }
  # }
  # lines(c(0, 1), c(0, 1), type = "l", lty = "longdash")
  # #lines(c(0, 0.85), c(0.15, 1), type = "l", lty = "dotdash")
  # #lines(c(0.15, 1), c(0, 0.85), type = "l", lty = "dotdash")
  # #lines(c(0.1, 0.1), c(0, 1), type = "l", lty = "dotdash")
  # lines(c(0.90, 0.90), c(0, 1), type = "l", lty = "dotdash")
  # #lines(c(0.05, 0.05), c(0, 1), type = "l", lty = "dotdash")
  # #lines(c(0.95, 0.95), c(0, 1), type = "l", lty = "dotdash")
  # #lines(c(0, 1), c(0.1, 0.1), type = "l", lty = "dotdash")
  # lines(c(0, 1), c(0.90, 0.90), type = "l", lty = "dotdash")
  # #lines(c(0, 1), c(0.05, 0.05), type = "l", lty = "dotdash")
  # #lines(c(0, 1), c(0.95, 0.95), type = "l", lty = "dotdash")
  # if(length(legends) >= 1){legend("topleft", legend = legends, pch = pchs, col = cols)}
  # out <- dev.off()
  # 
  # png(paste0("../data/analysis/features/", dataName, "-r2_ComparisonLowFiCorrVsWeightedHighFiCorr.png"), width = mult*682, height = mult*682)
  # plot(c(), c(),
  #      col = "black",
  #      type = "p",
  #      ylab = "Weighted correlation low (r2)",
  #      xlab = "Weighted correlation high (r2)",
  #      ylim = c(-0.1, 1.1),
  #      xlim = c(-0.1, 1.1),
  #      # xaxt = "n",
  #      pch = 20)
  # pchs <- c()
  # cols <- c()
  # legends <- c()
  # for(pch in unique(processedData$pch)){
  #   for(col in unique(processedData[processedData$pch == pch, "col"])){
  #     for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #       points(processedData[processedData$pch == pch & processedData$col == col, "highFiLowCorrelation_R2"], processedData[processedData$pch == pch & processedData$col == col, "lowFiLowCorrelation_R2"], pch = pch, col = col)
  #       pchs <- c(pchs, pch)
  #       cols <- c(cols, col)
  #       if(legend != ""){legends <- c(legends, legend)}
  #     }
  #   }
  # }
  # lines(c(0, 1), c(0, 1), type = "l", lty = "longdash")
  # #lines(c(0, 0.85), c(0.15, 1), type = "l", lty = "dotdash")
  # #lines(c(0.15, 1), c(0, 0.85), type = "l", lty = "dotdash")
  # #lines(c(0.1, 0.1), c(0, 1), type = "l", lty = "dotdash")
  # lines(c(0.90, 0.90), c(0, 1), type = "l", lty = "dotdash")
  # #lines(c(0.05, 0.05), c(0, 1), type = "l", lty = "dotdash")
  # #lines(c(0.95, 0.95), c(0, 1), type = "l", lty = "dotdash")
  # #lines(c(0, 1), c(0.1, 0.1), type = "l", lty = "dotdash")
  # lines(c(0, 1), c(0.90, 0.90), type = "l", lty = "dotdash")
  # #lines(c(0, 1), c(0.05, 0.05), type = "l", lty = "dotdash")
  # #lines(c(0, 1), c(0.95, 0.95), type = "l", lty = "dotdash")
  # if(length(legends) >= 1){legend("topleft", legend = legends, pch = pchs, col = cols)}
  # out <- dev.off()
  # 
  # png(paste0("../data/analysis/features/", dataName, "-r2_ComparisonGlobalCorrVsRelativeError.png"), width = mult*682, height = mult*682)
  # plot(c(), c(),
  #      col = "black",
  #      type = "p",
  #      #ylab = as.expression(bquote(CC^2)),
  #      ylab = "CC",
  #      xlab = "RRMSE",
  #      ylim = c(-0.1, 1.1),
  #      xlim = c(-0.1, 1.1 * max(as.numeric(processedData$relativeError))),
  #      # xaxt = "n",
  #      pch = 20)
  # pchs <- c()
  # cols <- c()
  # legends <- c()
  # for(pch in unique(processedData$pch)){
  #   for(col in unique(processedData[processedData$pch == pch, "col"])){
  #     for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #       points(processedData[processedData$pch == pch & processedData$col == col, "relativeError"], processedData[processedData$pch == pch & processedData$col == col, "globalCorrelation_R2"], pch = pch, col = col)
  #       pchs <- c(pchs, pch)
  #       cols <- c(cols, col)
  #       if(legend != ""){legends <- c(legends, legend)}
  #     }
  #   }
  # }
  # if(length(legends) >= 1){legend("topright", legend = legends, pch = pchs, col = cols)}
  # out <- dev.off()
  # 
  # for(corr in corrs){
  #   png(paste0("../data/analysis/features/", dataName, "-r2_ComparisonGlobalCorrVsLocalCorr-", corr, ".png"), width = mult*682, height = mult*682)
  #   plot(c(), c(),
  #        col = "black",
  #        type = "p",
  #        #ylab = as.expression(bquote(CC^2)),
  #        ylab = "CC",
  #        #xlab = paste0("Probability (local correlation)^2 > ", corr),
  #        xlab = as.expression(bquote(LCC*textstyle(atop(0.2, .(corr))))),
  #        ylim = c(-0.1, 1.1),
  #        xlim = c(-0.1, 1.1),
  #        # xaxt = "n",
  #        pch = 20)
  #   pchs <- c()
  #   cols <- c()
  #   legends <- c()
  #   for(pch in unique(processedData$pch)){
  #     for(col in unique(processedData[processedData$pch == pch, "col"])){
  #       for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #         points(processedData[processedData$pch == pch & processedData$col == col, paste0("localCorrelationR2_corr", corr)], processedData[processedData$pch == pch & processedData$col == col, "globalCorrelation_R2"], pch = pch, col = col)
  #         pchs <- c(pchs, pch)
  #         cols <- c(cols, col)
  #         if(legend != ""){legends <- c(legends, legend)}
  #       }
  #     }
  #   }
  #   lines(c(-0.1, 1.1), c(corr, corr), type = "l", lty = "longdash")
  #   if(length(legends) >= 1){legend("bottomright", legend = legends, pch = pchs, col = cols, horiz=TRUE)}
  #   out <- dev.off()
  #   
  #   png(paste0("../data/analysis/features/", dataName, "-r2_ComparisonLocalCoeffOfVariationVsLocalCorr-", corr, ".png"), width = mult*682, height = mult*682)
  #   plot(c(), c(),
  #        col = "black",
  #        type = "p",
  #        #ylab = "Local correlation Coefficient of Variation (r2)",
  #        ylab = as.expression(bquote(LCC*textstyle(atop(0.2, "coeff")))),
  #        #xlab = paste0("Probability (local correlation)^2 > ", corr),
  #        xlab = as.expression(bquote(LCC*textstyle(atop(0.2, .(corr))))),
  #        ylim = c(-0.1, 2.5),
  #        xlim = c(-0.1, 1.1),
  #        # xaxt = "n",
  #        pch = 20,
  #        mgp=c(2.5,1,0))
  #   pchs <- c()
  #   cols <- c()
  #   legends <- c()
  #   for(pch in unique(processedData$pch)){
  #     for(col in unique(processedData[processedData$pch == pch, "col"])){
  #       for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #         points(processedData[processedData$pch == pch & processedData$col == col, paste0("localCorrelationR2_corr", corr)], processedData[processedData$pch == pch & processedData$col == col, "localCorrelationCoefficientOfVariation_R2"], pch = pch, col = col)
  #         pchs <- c(pchs, pch)
  #         cols <- c(cols, col)
  #         if(legend != ""){legends <- c(legends, legend)}
  #       }
  #     }
  #   }
  #   if(length(legends) >= 1){legend("topright", legend = legends, pch = pchs, col = cols)}
  #   out <- dev.off()
  #   
  #   png(paste0("../data/analysis/features/", dataName, "-r2_ComparisonLocalVariationSDVsLocalCorr-", corr, ".png"), width = mult*682, height = mult*682)
  #   plot(c(), c(),
  #        col = "black",
  #        type = "p",
  #        #ylab = "Local correlation SD (r2)",
  #        ylab = as.expression(bquote(LCC*textstyle(atop(0.2, "sd")))),
  #        #xlab = paste0("Probability (local correlation)^2 > ", corr),
  #        xlab = as.expression(bquote(LCC*textstyle(atop(0.2, .(corr))))),
  #        ylim = c(-0.1, 1.1),
  #        xlim = c(-0.1, 1.1),
  #        # xaxt = "n",
  #        pch = 20,
  #        mgp=c(2.5,1,0))
  #   pchs <- c()
  #   cols <- c()
  #   legends <- c()
  #   for(pch in unique(processedData$pch)){
  #     for(col in unique(processedData[processedData$pch == pch, "col"])){
  #       for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #         points(processedData[processedData$pch == pch & processedData$col == col, paste0("localCorrelationR2_corr", corr)], processedData[processedData$pch == pch & processedData$col == col, "localCorrelationSD_R2"], pch = pch, col = col)
  #         pchs <- c(pchs, pch)
  #         cols <- c(cols, col)
  #         if(legend != ""){legends <- c(legends, legend)}
  #       }
  #     }
  #   }
  #   if(length(legends) >= 1){legend("topright", legend = legends, pch = pchs, col = cols)}
  #   out <- dev.off()
  # }
  
  
  
  
  
  
  # 
  # 
  # png(paste0("../data/analysis/features/", dataName, "-ComparisonGlobalCorrVsLocalCoeffOfVariation.png"), width = mult*682, height = mult*682)
  # plot(c(), c(),
  #      col = "black",
  #      type = "p",
  #      ylab = "Global correlation",
  #      xlab = "Local coefficient of variation",
  #      ylim = c(-1.1, 1.1),
  #      xlim = c(-0.1, 2.5),
  #      # xaxt = "n",
  #      pch = 20)
  # pchs <- c()
  # cols <- c()
  # legends <- c()
  # for(pch in unique(processedData$pch)){
  #   for(col in unique(processedData[processedData$pch == pch, "col"])){
  #     for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #       points(processedData[processedData$pch == pch & processedData$col == col, "localCorrelationCoefficientOfVariation"], processedData[processedData$pch == pch & processedData$col == col, "globalCorrelation"], pch = pch, col = col)
  #       pchs <- c(pchs, pch)
  #       cols <- c(cols, col)
  #       if(legend != ""){legends <- c(legends, legend)}
  #     }
  #   }
  # }
  # if(length(legends) >= 1){legend("topright", legend = legends, pch = pchs, col = cols)}
  # out <- dev.off()
  # 
  # png(paste0("../data/analysis/features/", dataName, "-ComparisonGlobalCorrVsLocalVariationSD.png"), width = mult*682, height = mult*682)
  # plot(c(), c(),
  #      col = "black",
  #      type = "p",
  #      ylab = "Global correlation",
  #      xlab = "Local variation SD",
  #      ylim = c(-1.1, 1.1),
  #      xlim = c(-0.1, 1.1),
  #      # xaxt = "n",
  #      pch = 20)
  # pchs <- c()
  # cols <- c()
  # legends <- c()
  # for(pch in unique(processedData$pch)){
  #   for(col in unique(processedData[processedData$pch == pch, "col"])){
  #     for(legend in unique(processedData[processedData$pch == pch & processedData$col == col, "legend"])){
  #       points(processedData[processedData$pch == pch & processedData$col == col, "localCorrelationVariance"], processedData[processedData$pch == pch & processedData$col == col, "globalCorrelation"], pch = pch, col = col)
  #       pchs <- c(pchs, pch)
  #       cols <- c(cols, col)
  #       if(legend != ""){legends <- c(legends, legend)}
  #     }
  #   }
  # }
  # if(length(legends) >= 1){legend("topright", legend = legends, pch = pchs, col = cols)}
  # out <- dev.off()
  
}

#data <- getAveragePerformance("modelPerformanceSetup", 1, 832)
#currentData <- plotInstanceFeatures(data, "literatureAnalysis", c(0.85, 0.9, 0.95, 0.975))

#plotInstanceFeatures(fullData, "test", c(0.975))



# points(processedData[processedData$pch == 0, "localCorrelationSD"], processedData[processedData$pch == 0, "globalCorrelation"], pch = 0, col = "black")
# points(processedData[processedData$pch == 1, "localCorrelationSD"], processedData[processedData$pch == 1, "globalCorrelation"], pch = 1, col = "black")
# points(processedData[processedData$pch == 2, "localCorrelationSD"], processedData[processedData$pch == 2, "globalCorrelation"], pch = 2, col = "black")
# legend("topright", legend = c("Non-parametric, non-analysed instances", "Parametric, non-analysed instances", "Parametric, analysed instances"), pch = c(1, 2, 0), col = "black")




