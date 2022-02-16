source("R_code/libraries.R")
# Function which plots multiple F1 VS F2 plots, where F1 and F2 are features
# of the instances given in processedData. The legend shows the type of instance,
# i.e. fixed, parameter based, error based or disturbance based.
plotFeatures <- function(processedData, dataName, mult = 1.0, corrs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.975), legend = TRUE){
  pointSize <- 1
  smallDim <- 10
  # First check if data comes with a specification on pch and colour
  if(!("pch" %in% colnames(processedData))){processedData$pch = 1}
  if(!("col" %in% colnames(processedData))){processedData$col = "black"}
  if(!("legend" %in% colnames(processedData))){processedData$legend = ""}
  
  # Start with CC vs RRMSE
  tempData <- data.frame("CC" = as.numeric(processedData$CC),
                         "RRMSE" = as.numeric(processedData$RRMSE),
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
    ggsave(paste0("data/plots/featurePlots/", dataName, "-ComparisonCCVsRRMSE.png"),
           plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
           width = mult*18, height = mult*12, units = "cm")
  }else{
    p <- p + theme(legend.position="none")
    ggsave(paste0("data/plots/featurePlots/", dataName, "-ComparisonCCVsRRMSE.png"),
           plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
           width = mult*12, height = mult*12, units = "cm")
  }
  
  
  # Process LCC_p VS CC, LCC_coeff, LCC_sd for different p values
  for(corr in corrs){
    tempData <- data.frame("CC" = as.numeric(processedData$CC),
                           "LCC" = as.numeric(processedData[, paste0("LCC_", corr)]),
                           "LCC_coeff" = as.numeric(processedData$LCC_coeff),
                           "LCC_sd" = as.numeric(processedData$LCC_sd),
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
      ggsave(paste0("data/plots/featurePlots/", dataName, "-ComparisonCCVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*18, height = mult*12, units = "cm")
    }else{
      p <- p + theme(legend.position="none")
      ggsave(paste0("data/plots/featurePlots/", dataName, "-ComparisonCCVsLCC_", corr, ".png"),
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
      ggsave(paste0("data/plots/featurePlots/", dataName, "-ComparisonLCC_coeffVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*18, height = mult*12, units = "cm")
    }else{
      p <- p + theme(legend.position="none")
      ggsave(paste0("data/plots/featurePlots/", dataName, "-ComparisonLCC_coeffVsLCC_", corr, ".png"),
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
      ggsave(paste0("data/plots/featurePlots/", dataName, "-ComparisonLCC_sdVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*18, height = mult*12, units = "cm")
    }else{
      p <- p + theme(legend.position="none")
      ggsave(paste0("data/plots/featurePlots/", dataName, "-ComparisonLCC_sdVsLCC_", corr, ".png"),
             plot = set_panel_size(p, width  = unit(mult*smallDim, "cm"), height = unit(mult*smallDim, "cm")),
             width = mult*12, height = mult*12, units = "cm")
    }
  }
}





