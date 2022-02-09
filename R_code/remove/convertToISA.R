source("dataProcessor.R")
library(flacco)
library(RANN)
library(mda)
library(stringr)

createInstanceSpaceAnalysisData <- function(data, algorithmsNamesOrig, algorithmsNamesFinal, featuresNamesOrig, featuresFinal, addFeatures = FALSE, levels = c("high", "low", "mid")){
  
  featuresSetHigh <- c("cm_angle", "ela_distr", "ela_level", "ela_meta", "ic", "basic", "disp", "pca")
  featuresSetLow <- c("cm_angle", "ela_meta", "ic", "basic", "disp")
  featuresSetMid <- c("cm_angle", "ela_meta", "ic", "basic", "disp")
  #featuresSet <- c("cm_angle", "ela_level", "ela_meta", "ic", "basic", "disp")
  featureSetOutHigh <- c("cm_angle.dist_ctr2best.mean", "cm_angle.dist_ctr2worst.mean", "cm_angle.angle.mean",
                         "ela_distr.skewness", "ela_distr.kurtosis", "ela_distr.number_of_peaks",
                         "ela_level.mmce_lda_10", "ela_level.mmce_qda_10", "ela_level.mmce_lda_25",
                         "ela_level.mmce_qda_25", "ela_level.mmce_lda_50", "ela_level.mmce_qda_50",
                         "ela_meta.lin_simple.adj_r2", "ela_meta.lin_simple.intercept", "ela_meta.lin_simple.coef.min",
                         "ela_meta.lin_simple.coef.max", "ela_meta.lin_simple.coef.max_by_min", "ela_meta.lin_w_interact.adj_r2",
                         "ela_meta.quad_simple.adj_r2", "ela_meta.quad_simple.adj_r2", "ela_meta.quad_simple.cond", "ela_meta.quad_w_interact.adj_r2",
                         "ic.h.max", "ic.eps.max", "ic.eps.ratio", "ic.m0",
                         "basic.lower_min", "basic.lower_max", "basic.upper_min", "basic.upper_max", "basic.objective_min", "basic.objective_max",
                         "disp.ratio_mean_02", "disp.ratio_mean_05", "disp.ratio_mean_10", "disp.ratio_mean_25",
                         "disp.ratio_median_02", "disp.ratio_median_05", "disp.ratio_median_10", "disp.ratio_median_25",
                         "disp.diff_mean_02", "disp.diff_mean_05", "disp.diff_mean_10", "disp.diff_mean_25",
                         "disp.diff_median_02", "disp.diff_median_05", "disp.diff_median_10", "disp.diff_median_25",
                         "pca.expl_var.cov_x", "pca.expl_var.cor_x", "pca.expl_var.cov_init", "pca.expl_var.cor_init",
                         "pca.expl_var_PC1.cov_x", "pca.expl_var_PC1.cor_x", "pca.expl_var_PC1.cov_init", "pca.expl_var_PC1.cor_init")
  featureSetOutLow <- c("cm_angle.dist_ctr2best.mean", "cm_angle.dist_ctr2worst.mean", "cm_angle.angle.mean",
                        "ela_meta.lin_simple.intercept", "ela_meta.lin_simple.coef.min",
                        "ela_meta.lin_simple.coef.max",
                        "ic.h.max", "ic.eps.max", "ic.m0",
                        "basic.lower_min", "basic.lower_max", "basic.upper_min", "basic.upper_max", "basic.objective_min", "basic.objective_max",
                        "disp.ratio_mean_02", "disp.ratio_mean_05", "disp.ratio_mean_10", "disp.ratio_mean_25",
                        "disp.ratio_median_02", "disp.ratio_median_05", "disp.ratio_median_10", "disp.ratio_median_25",
                        "disp.diff_mean_02", "disp.diff_mean_05", "disp.diff_mean_10", "disp.diff_mean_25",
                        "disp.diff_median_02", "disp.diff_median_05", "disp.diff_median_10", "disp.diff_median_25")
  featureSetOutMid <- c("cm_angle.dist_ctr2best.mean", "cm_angle.dist_ctr2worst.mean", "cm_angle.angle.mean",
                        "ela_meta.lin_simple.intercept", "ela_meta.lin_simple.coef.min",
                        "ela_meta.lin_simple.coef.max",
                        "ic.h.max", "ic.eps.max", "ic.m0",
                        "basic.lower_min", "basic.lower_max", "basic.upper_min", "basic.upper_max", "basic.objective_min", "basic.objective_max",
                        "disp.ratio_mean_02", "disp.ratio_mean_05", "disp.ratio_mean_10", "disp.ratio_mean_25",
                        "disp.ratio_median_02", "disp.ratio_median_05", "disp.ratio_median_10", "disp.ratio_median_25",
                        "disp.diff_mean_02", "disp.diff_mean_05", "disp.diff_mean_10", "disp.diff_mean_25",
                        "disp.diff_median_10", "disp.diff_median_25")
  
  #cols <- 2 + length(algorithmsNamesOrig) + length(featuresNamesOrig) + 3*length(featureSetOut)*addFeatures
  #cols <- 2 + length(algorithmsNamesOrig) + length(featuresNamesOrig) + length(featureSetOutLow)*addFeatures*length(levels)
  cols <- 2 + length(algorithmsNamesOrig) + length(featuresNamesOrig)
  for(level in levels){
    if(level == "high"){cols <- cols + length(featureSetOutHigh)*addFeatures}
    if(level == "low"){cols <- cols + length(featureSetOutLow)*addFeatures}
    if(level == "mid"){cols <- cols + length(featureSetOutMid)*addFeatures}
  }
  runData <- data.frame(matrix(ncol = cols, nrow = 0))
  instances <- unique(data$instance)
  if(length(instances) != length(data$instance)){
    print(paste0("Got rows with repeated instances, should not happen! Got ", length(data$instance), " instances but only ", length(instances), " unique instances"))
  }
  row <- 0
  for(instance in instances){
    row <- row + 1
    cat(paste0("\rWorking on instance ", row, "/", length(instances), ", instance ", instance, "..."))
    vals <- c(instance, "Fixed")
    for(featureName in featuresNamesOrig){
      vals <- c(vals, data[data$instance == instance, featureName])
    }
    
    # Add new features if want them
    if(addFeatures){
      instanceName <- tail(strsplit(instance, "_")[[1]], 1)
      #for(level in c("high", "low", "mid")){
      for(level in levels){
        if(level == "high"){
          featuresSet <- featuresSetHigh
          featureSetOut <- featureSetOutHigh
        }else if(level == "low"){
          featuresSet <- featuresSetLow
          featureSetOut <- featureSetOutLow
        }else if(level == "mid"){
          featuresSet <- featuresSetMid
          featureSetOut <- featureSetOutMid
        }
        
        cat(paste0(level,"-"))
        samples <- read.csv(paste0("../data/sampledFunctions/", instanceName, "-", level, ".txt"))
        dim <- ncol(samples) - 1
        if(dim == 1){
          X <- data.frame(samples$X0)
        }else{
          X <- samples[, 1:dim]
        }
        Y <- samples[, dim+1]
        feat.object = createFeatureObject(X = X, y = Y)
        allFeatures <- c()
        for(set in featuresSet){
          cat(paste0(set, "_"))
          allFeatures <- c(allFeatures, calculateFeatureSet(feat.object = feat.object, set = set))
        }
        allFeatures <- allFeatures[featureSetOut]
        vals <- c(vals, allFeatures)
      }
    }
    
    for(algorithmName in algorithmsNamesOrig){
      colName <- paste0(algorithmName, "-median")
      vals <- c(vals, data[data$instance == instance, colName])
    }
    runData[row, ] <- vals
  }
  cat("done!\n")
  # Get final column names
  colNames <- c("instances", "Source")
  for(name in featuresFinal){
    colNames <- c(colNames, paste0("feature_", name))
  }
  if(addFeatures){
    #for(level in c("high", "low", "mid")){
    for(level in levels){
      if(level == "high"){
        featuresSet <- featuresSetHigh
        featureSetOut <- featureSetOutHigh
      }else if(level == "low"){
        featuresSet <- featuresSetLow
        featureSetOut <- featureSetOutLow
      }else if(level == "mid"){
        featuresSet <- featuresSetMid
        featureSetOut <- featureSetOutMid
      }
      for(singleFeature in featureSetOut){
        colNames <- c(colNames, paste0("feature_", level, "-", singleFeature))
      }
    }
  }
  for(name in algorithmsNamesFinal){
    colNames <- c(colNames, paste0("algo_", name))
  }
  colnames(runData) <- colNames
  runData[str_which(runData$instances, "Toal", negate = FALSE), "Source"] = "Parameter-based"
  runData[str_which(runData$instances, "Wang", negate = FALSE), "Source"] = "Error-based"
  runData[str_which(runData$instances, "COCO", negate = FALSE), "Source"] = "Disturbance-based"
  
  runData[, 1] <- as.character(runData[, 1])
  runData[, 2] <- as.character(runData[, 2])
  for(col in 3:ncol(runData)){
    runData[, col] <- as.numeric(runData[, col])
  }
  # Add source to data
  #runData$Source <- "Fixed"
  
  return(runData)
}

# Uncomment and process this if first time, but takes forever!
#corrs <- c(seq(0.1, 0.9, 0.1), 0.95, 0.975)
#litSuiteModelFit <- combineAndProcessResults("litSuiteSurrogateModelFitExpandedCoKrig", 1, 2265, localCorrs = corrs)
#augSuiteModelFit <- combineAndProcessResults("finalModelAccuracySuiteSurrogateModelFitExpandedCoKrig", 1, 14280, localCorrs = corrs)

# Save augSuiteModelFit, and remove function12-dim1- as numerical errors occured
#savedAugData <- augSuiteModelFit
#augSuiteModelFit <- augSuiteModelFit[str_which(augSuiteModelFit$instance, "function12-dim1-", negate = TRUE),]


# DONE AGAIN, WITH ALL FEATURES TO ADD CLARITY
algorithms <- c("kriging_1_0", "cokriging_0.8_0.2")
algorithmsName <- c("Kriging", "Co-Kriging")
features <- c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError", "localCorrelationSD_R2", "localCorrelationCoefficientOfVariation_R2")
featuresName <- c("dimension", "budget", "C_r", "CC", "RRMSE", "LCC_sd", "LCC_coeff")
for(corr in corrs){
  features <- c(features, paste0("localCorrelationR2_corr", corr))
  featuresName <- c(featuresName, paste0("LCC_", corr))
}
#resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName, TRUE, c("high", "low", "mid"))
#resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName, TRUE, c("high", "low", "mid"))

#resultLitSaved <- resultLit
#resultAugSaved <- resultAug
#resultLitManyKrigSaved <- resultLitManyKrig
#resultAugManyKrigSaved <- resultAugManyKrig

resultLit <- resultLitSaved
resultAug <- resultAugSaved
resultLitManyKrig <- resultLitManyKrigSaved
resultAugManyKrig <- resultAugManyKrigSaved

# Check what names are there
removeNames <- c("feature_dimension", "feature_budget", "feature_C_r", "feature_CC", "feature_RRMSE", "feature_LCC_sd", "feature_LCC_coeff")
for(corr in corrs){
  removeNames <- c(removeNames, paste0("feature_LCC_", corr))
}
removeNames <- c()
for(name in colNames){
  if(length(resultLit[is.infinite(resultLit[, name]), name]) > 0 ||
      length(resultLit[is.infinite(resultAug[, name]), name]) > 0 ||
      length(resultLit[is.na(resultLit[, name]), name]) > 0 ||
      length(resultLit[is.na(resultAug[, name]), name]) > 0){removeNames <- c(removeNames, name)}
}

resultLit <- resultLit[ , ! names(resultLit) %in% removeNames]
resultAug <- resultAug[ , ! names(resultAug) %in% removeNames]
resultLitManyKrig <- cbind(resultLit, resultLitManyKrig[, c("algo_Co-Kriging(h-0.6_l-0.4)", "algo_Co-Kriging(h-0.4_l-0.6)", "algo_Co-Kriging(h-0.2_l-0.8)")])
resultAugManyKrig <- cbind(resultAug, resultAugManyKrig[, c("algo_Co-Kriging(h-0.6_l-0.4)", "algo_Co-Kriging(h-0.4_l-0.6)", "algo_Co-Kriging(h-0.2_l-0.8)")])


write.table(resultLit, "../data/instanceSpaceAnalysisInput/litSuiteAllFeatures.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/augSuiteAllFeatures.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(resultLitManyKrig, "../data/instanceSpaceAnalysisInput/litSuiteAllFeaturesManyCoKrig.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLitManyKrig, resultAugManyKrig), "../data/instanceSpaceAnalysisInput/augSuiteAllFeaturesManyCoKrig.csv", quote = FALSE, row.names = FALSE, sep = ",")



# NEW WAY, WITHOUT FEATURE SELECTION
corrs <- c(seq(0.2, 0.8, 0.2), 0.95)
# First instance space analysis of algorithms Kriging vs Co-Kriging
algorithms <- c("kriging_1_0", "cokriging_0.8_0.2")
algorithmsName <- c("Kriging", "Co-Kriging")
# First look at how it would go with limited features, without CC
features <- c("dimension", "relativeError")
featuresName <- c("dimension", "RRMSE")

resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/01-litSuiteBasicFeaturesNoCC.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/05-augSuiteBasicFeaturesNoCC.csv", quote = FALSE, row.names = FALSE, sep = ",")

# Now show impact of CC
features <- c("dimension", "globalCorrelation_R2", "relativeError")
featuresName <- c("dimension", "CC", "RRMSE")

resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/02-litSuiteBasicFeaturesWithCC.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/06-augSuiteBasicFeaturesWithCC.csv", quote = FALSE, row.names = FALSE, sep = ",")

# Now look at all features, chose them for the selector
features <- c("globalCorrelation_R2", "relativeError", "localCorrelationSD_R2", "localCorrelationCoefficientOfVariation_R2")
featuresName <- c("CC", "RRMSE", "LCC_sd", "LCC_coeff")
for(corr in corrs){
  features <- c(features, paste0("localCorrelationR2_corr", corr))
  featuresName <- c(featuresName, paste0("LCC_", corr))
}
resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/03-litSuiteAllFeatures.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/07-augSuiteAllFeatures.csv", quote = FALSE, row.names = FALSE, sep = ",")

# One last one with further reduced new features to see if can get similar accuracy
features <- c("globalCorrelation_R2", "relativeError", "localCorrelationSD_R2")
featuresName <- c("CC", "RRMSE", "LCC_sd")
for(corr in c(0.2, 0.4, 0.8)){
  features <- c(features, paste0("localCorrelationR2_corr", corr))
  featuresName <- c(featuresName, paste0("LCC_", corr))
}
resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/04-litSuiteAllFeaturesReduced.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/08-augSuiteAllFeatures.csv", quote = FALSE, row.names = FALSE, sep = ",")



# Now repeat but with Krig vs Co-Krig with multiple ratios
algorithms <- c("kriging_1_0", "cokriging_0.8_0.2", "cokriging_0.6_0.4", "cokriging_0.4_0.6", "cokriging_0.2_0.8")
algorithmsName <- c("Kriging", "Co-Kriging(h-0.8_l-0.2)", "Co-Kriging(h-0.6_l-0.4)", "Co-Kriging(h-0.4_l-0.6)", "Co-Kriging(h-0.2_l-0.8)")
# First look at how it would go with limited features, without CC
features <- c("dimension", "relativeError")
featuresName <- c("dimension", "RRMSE")

resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/09-litSuiteBasicFeaturesNoCCManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/13-augSuiteBasicFeaturesNoCCManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")

# Now show impact of CC
features <- c("dimension", "globalCorrelation_R2", "relativeError")
featuresName <- c("dimension", "CC", "RRMSE")

resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/10-litSuiteBasicFeaturesWithCCManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/14-augSuiteBasicFeaturesWithCCManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")

# Now look at all features, chose them for the selector
features <- c("globalCorrelation_R2", "relativeError", "localCorrelationSD_R2", "localCorrelationCoefficientOfVariation_R2")
featuresName <- c("CC", "RRMSE", "LCC_sd", "LCC_coeff")
for(corr in corrs){
  features <- c(features, paste0("localCorrelationR2_corr", corr))
  featuresName <- c(featuresName, paste0("LCC_", corr))
}
resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/11-litSuiteAllFeaturesManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/15-augSuiteAllFeaturesManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")

# One last one with further reduced new features to see if can get similar accuracy
features <- c("globalCorrelation_R2", "relativeError", "localCorrelationSD_R2")
featuresName <- c("CC", "RRMSE", "LCC_sd")
for(corr in c(0.2, 0.5, 0.8)){
  features <- c(features, paste0("localCorrelationR2_corr", corr))
  featuresName <- c(featuresName, paste0("LCC_", corr))
}
resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/12-litSuiteAllFeaturesReducedManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/16-augSuiteAllFeaturesManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")
# END NEW WAY


# OLD WAY
corrs <- c(seq(0.1, 0.9, 0.1), 0.95, 0.975)
# First instance space analysis of algorithms Kriging vs Co-Kriging
algorithms <- c("kriging_1_0", "cokriging_0.8_0.2")
algorithmsName <- c("Kriging", "Co-Kriging")
# First look at how it would go with limited features
features <- c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError")
featuresName <- c("dimension", "budget", "C_r", "CC", "RRMSE")

resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/01-litSuiteBasicFeatures.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/03-augSuiteBasicFeatures.csv", quote = FALSE, row.names = FALSE, sep = ",")

# Do an extra one with lit features but no RRMSE
features <- c("dimension", "budget", "costRatio", "globalCorrelation_R2")
featuresName <- c("dimension", "budget", "C_r", "CC")

resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/09-litSuiteBasicFeaturesNoRRMSE.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/10-augSuiteBasicFeaturesNoRRMSE.csv", quote = FALSE, row.names = FALSE, sep = ",")


# Now look at all features
features <- c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError", "localCorrelationSD_R2", "localCorrelationCoefficientOfVariation_R2")
featuresName <- c("dimension", "budget", "C_r", "CC", "RRMSE", "LCC_sd", "LCC_coeff")
for(corr in corrs){
  features <- c(features, paste0("localCorrelationR2_corr", corr))
  featuresName <- c(featuresName, paste0("LCC_", corr))
}
resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/02-litSuiteAllFeatures.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/04-augSuiteAllFeatures.csv", quote = FALSE, row.names = FALSE, sep = ",")



# Now repeat but with Krig vs Co-Krig with multiple ratios
algorithms <- c("kriging_1_0", "cokriging_0.8_0.2", "cokriging_0.6_0.4", "cokriging_0.4_0.6", "cokriging_0.2_0.8")
algorithmsName <- c("Kriging", "Co-Kriging(h-0.8_l-0.2)", "Co-Kriging(h-0.6_l-0.4)", "Co-Kriging(h-0.4_l-0.6)", "Co-Kriging(h-0.2_l-0.8)")
# First look at how it would go with limited features
features <- c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError")
featuresName <- c("dimension", "budget", "C_r", "CC", "RRMSE")

resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/05-litSuiteBasicFeaturesManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/07-augSuiteBasicFeaturesManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")

# Do an extra one with lit features but no RRMSE
features <- c("dimension", "budget", "costRatio", "globalCorrelation_R2")
featuresName <- c("dimension", "budget", "C_r", "CC")

resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/11-litSuiteBasicFeaturesNoRRMSEManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/12-augSuiteBasicFeaturesNoRRMSEManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")



# Now look at all features
features <- c("dimension", "budget", "costRatio", "globalCorrelation_R2", "relativeError", "localCorrelationSD_R2", "localCorrelationCoefficientOfVariation_R2")
featuresName <- c("dimension", "budget", "C_r", "CC", "RRMSE", "LCC_sd", "LCC_coeff")
for(corr in corrs){
  features <- c(features, paste0("localCorrelationR2_corr", corr))
  featuresName <- c(featuresName, paste0("LCC_", corr))
}
resultLit <- createInstanceSpaceAnalysisData(litSuiteModelFit, algorithms, algorithmsName, features, featuresName)
resultAug <- createInstanceSpaceAnalysisData(augSuiteModelFit, algorithms, algorithmsName, features, featuresName)
write.table(resultLit, "../data/instanceSpaceAnalysisInput/06-litSuiteAllFeaturesManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")
write.table(rbind(resultLit, resultAug), "../data/instanceSpaceAnalysisInput/08-augSuiteAllFeaturesManyCokrig.csv", quote = FALSE, row.names = FALSE, sep = ",")



# result <- createInstanceSpaceAnalysisData(dataset01_litSuiteSurrogateModelFit, algorithms, algorithmsName, features, featuresName)
# write.table(result, "../data/instanceSpaceAnalysisInput/litInstancesBasic.csv", quote = FALSE, row.names = FALSE, sep = ",")
# 
# result <- rbind(createInstanceSpaceAnalysisData(dataset01_litSuiteSurrogateModelFit, algorithms, algorithmsName, features, featuresName),
#                 createInstanceSpaceAnalysisData(dataset02_finalModelAccuracySuiteSurrogateModelFit, algorithms, algorithmsName, features, featuresName))
# write.table(result, "../data/instanceSpaceAnalysisInput/augInstancesBasic.csv", quote = FALSE, row.names = FALSE, sep = ",")


# # START OF PLAYING AROUND HERE
# # Get processed results, old should be absolute performance, new should be relative
# rawDataOld <- combineArrayResults("00-finalModelAccuracySuiteSurrogateModelFit", 1, 1143, 5)
# rawDataNew <- combineArrayResults("finalModelAccuracySuiteSurrogateModelFitExpandedCoKrig", 1, 14280)
# # Save data and look for functions of interest for processed data
# processedDataNewSubset <- augSuiteModelFit
# processedDataNewSubset <- processedDataNewSubset[str_which(processedDataNewSubset$instance, "function12-dim1-", negate = TRUE),]
# processedDataNewSubset <- processedDataNewSubset[processedDataNewSubset$`cokriging_0.8_0.2-mean` > 1, ]
# processedDataNewSubset <- processedDataNewSubset[, c("instance", "cokriging_0.8_0.2-mean", "kriging_1_0-mean")]
# processedDataOldSubset <- dataset02_finalModelAccuracySuiteSurrogateModelFit
# processedDataOldSubset <- processedDataOldSubset[processedDataOldSubset$instance %in% processedDataNewSubset$instance, c("instance", "cokriging_0.8_0.2-mean")]
# # Do the same for raw data
# rawDataOldInterest <- rawDataOld
# rawDataNewInterest <- rawDataNew
# rawDataNewInterest <- rawDataNewInterest[rawDataNewInterest$initialBudgetHigh == 1 |
#                                            rawDataNewInterest$initialBudgetHigh == 0.8, ]
# # rawDataNewInterest <- rawDataNewInterest[rawDataNewInterest$performance > 1, ]
# 
# # rawDataOldInterest <- rawDataOldInterest[rawDataOldInterest$technique == "cokriging" & 
# #                                            rawDataOldInterest$initialBudgetHigh == 0.8, ]
# # rawDataNewInterest <- rawDataNewInterest[rawDataNewInterest$technique == "cokriging" & 
# #                                            rawDataNewInterest$initialBudgetHigh == 0.8, ]
# processedDataNewSubset$instance <- as.character(processedDataNewSubset$instance)
# processedDataOldSubset$instance <- as.character(processedDataOldSubset$instance)
# rawDataNewInterest$instance <- as.character(rawDataNewInterest$instance)
# rawDataOldInterest$instance <- as.character(rawDataOldInterest$instance)
# 
# for(i in 1:nrow(processedDataNewSubset)){
#   print(substr(processedDataNewSubset[i, "instance"], 22, 320))
#   if(i == 1){chosenOldRawData <- rawDataOldInterest[str_which(rawDataOldInterest$instance, substr(processedDataNewSubset[i, "instance"], 25, 320), negate = FALSE), ]}
#   else{chosenOldRawData <- rbind(chosenOldRawData, rawDataOldInterest[str_which(rawDataOldInterest$instance, substr(processedDataNewSubset[i, "instance"], 25, 320), negate = FALSE), ])}
# }
# 
# for(i in 1:nrow(processedDataNewSubset)){
#   print(substr(processedDataNewSubset[i, "instance"], 22, 320))
#   if(i == 1){chosenNewRawData <- rawDataNewInterest[str_which(rawDataNewInterest$instance, substr(processedDataNewSubset[i, "instance"], 25, 320), negate = FALSE), ]}
#   else{chosenNewRawData <- rbind(chosenNewRawData, rawDataNewInterest[str_which(rawDataNewInterest$instance, substr(processedDataNewSubset[i, "instance"], 25, 320), negate = FALSE), ])}
# }
# # Do the same but for kriging this time
# rawDataOldInterestKrig <- rawDataOld
# rawDataNewInterestKrig <- rawDataNew
# rawDataOldInterestKrig <- rawDataOldInterestKrig[rawDataOldInterestKrig$technique == "kriging", ]
# rawDataNewInterestKrig <- rawDataNewInterestKrig[rawDataNewInterestKrig$technique == "kriging", ]
# processedDataNewSubset$instance <- as.character(processedDataNewSubset$instance)
# processedDataOldSubset$instance <- as.character(processedDataOldSubset$instance)
# rawDataNewInterestKrig$instance <- as.character(rawDataNewInterestKrig$instance)
# rawDataOldInterestKrig$instance <- as.character(rawDataOldInterestKrig$instance)
# 
# for(i in 1:nrow(processedDataNewSubset)){
#   print(substr(processedDataNewSubset[i, "instance"], 22, 320))
#   if(i == 1){chosenOldRawDataKrig <- rawDataOldInterestKrig[str_which(rawDataOldInterestKrig$instance, substr(processedDataNewSubset[i, "instance"], 25, 320), negate = FALSE), ]}
#   else{chosenOldRawDataKrig <- rbind(chosenOldRawDataKrig, rawDataOldInterestKrig[str_which(rawDataOldInterestKrig$instance, substr(processedDataNewSubset[i, "instance"], 25, 320), negate = FALSE), ])}
# }
# 
# for(i in 1:nrow(processedDataNewSubset)){
#   print(substr(processedDataNewSubset[i, "instance"], 22, 320))
#   if(i == 1){chosenNewRawDataKrig <- rawDataNewInterestKrig[str_which(rawDataNewInterestKrig$instance, substr(processedDataNewSubset[i, "instance"], 25, 320), negate = FALSE), ]}
#   else{chosenNewRawDataKrig <- rbind(chosenNewRawDataKrig, rawDataNewInterestKrig[str_which(rawDataNewInterestKrig$instance, substr(processedDataNewSubset[i, "instance"], 25, 320), negate = FALSE), ])}
# }
# # Modified function 12, here is check that all is well
# rawDataModifiedFinal <- combineArrayResults("miniTest", 1, 120)
# 
# 
# # Finally, go through all of the files augSuiteModelFit <- combineAndProcessResults("finalModelAccuracySuiteSurrogateModelFitExpandedCoKrig", 1, 14280, localCorrs = corrs)
# # and find which ones had COCOfunction12 as need to rerun them
# runs <- c()
# for(i in 1:14280){
#   print(i)
#   data <- read.table(paste0("../data/clusterResults/finalModelAccuracySuiteSurrogateModelFitExpandedCoKrig_arrayJob", i, ".txt"), header = TRUE, sep = " ", fill = TRUE)
#   if(length(str_which(data$instance, "COCOfunction12", negate = FALSE)) > 0){runs <- c(runs, i)}
# }
# 
# string <- ""
# for(run in runs){
#   if(run < 10000){next}
#   else{run <- run - 10000}
#   string <- paste(string, ",", sep="")
#   string <- paste(string, as.character(run), sep="")
# }
# string
# length(runs)
# # MESSING AROUND ENDS HERE

# See if can calculate extra features
#samples <- read.csv("../data/sampledFunctions/ToalHartmannH30.30-mid.txt")
file <- "../data/sampledFunctions/LiuPedagogical-high.txt"

samples <- read.csv(file)
dim <- ncol(samples) - 1
if(dim == 1){
  X <- data.frame(samples$X0)
}else{
  X <- samples[, 1:dim]
}
Y <- samples[, dim+1]
feat.object = createFeatureObject(X = X, y = Y)
allFeatures <- c()
for(set in featuresSet){
  allFeatures <- c(allFeatures, calculateFeatureSet(feat.object = feat.object, set = set))
}
allFeatures <- allFeatures[featureSetOut]
done <- calculateFeatureSet(feat.object = feat.object, set = "pca")
features
listAvailableFeatureSets()
?calculateFeatureSet
