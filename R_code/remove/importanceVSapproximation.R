source("modelBuildingScript.R")
library(flacco)
library(RANN)
library(mda)
library(stringr)
library(ggplot2)
library(ramify)

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

storeApproximations <- function(realFeaturesData, approxs, seeds){
  basicData <- realFeaturesData
  approximationAugmentedData <- basicData
  for(instance in unique(basicData$instances)){
    instanceName <- tail(strsplit(instance, "_")[[1]], 1)
    # Now do analysis for each approximation
    for(approx in approxs){
      for(seed in seeds){
        # First save the data that is the same
        approximationAugmentedData[nrow(approximationAugmentedData) + 1, c("instances", "Source", "feature_dimension", "feature_budget", "feature_C_r")] <-
          c(paste0(instance, "_samples-", approx, "_seed-", seed), approximationAugmentedData[approximationAugmentedData$instances == instance, c("Source", "feature_dimension", "feature_budget", "feature_C_r")])
        # Here should open file and get sample, and recalculate feature
        print(paste0("Working on instance ", instanceName, " (", match(instance, unique(basicData$instances)), "/", length(unique(basicData$instances)), "), samples ", approx, " and seed ", seed))
        # First read CC, RRMSE, and calculate all LCC
        print(paste0("Open ", "../data/sampledFunctions/", instanceName, "_basicFeat_samples-", approx, "_seed-", seed, ".txt"))
        simpleVals <- read.csv(paste0("../data/sampledFunctions/", instanceName, "_basicFeat_samples-", approx, "_seed-", seed, ".txt"))
        approximationAugmentedData[nrow(approximationAugmentedData), c("feature_CC", "feature_RRMSE")] <- c(simpleVals[1, "cc"]^2, simpleVals[1, "rrmse"])
        # Need to process LCC
        lccs <- calculate_local_probabilites(simpleVals$lcc, corrs)
        approximationAugmentedData[nrow(approximationAugmentedData), c("feature_LCC_sd", "feature_LCC_coeff")] <- lccs[1, c("localCorrelationSD_R2", "localCorrelationCoeffVariation_R2")]
        for(corr in corrs){
          approximationAugmentedData[nrow(approximationAugmentedData), paste0("feature_LCC_", corr)] <- lccs[1, paste0("localCorrR2_", corr)]
        }
        print(paste0("Open ", "../data/sampledFunctions/", instanceName, "-high_samples-", approx, "_seed-", seed, ".txt"))
        samplesHigh <- read.csv(paste0("../data/sampledFunctions/", instanceName, "-high_samples-", approx, "_seed-", seed, ".txt"))
        print(paste0("Open ", "../data/sampledFunctions/", instanceName, "-low_samples-", approx, "_seed-", seed, ".txt"))
        samplesLow <- read.csv(paste0("../data/sampledFunctions/", instanceName, "-low_samples-", approx, "_seed-", seed, ".txt"))
        print(paste0("Open ", "../data/sampledFunctions/", instanceName, "-mid_samples-", approx, "_seed-", seed, ".txt"))
        samplesMid <- read.csv(paste0("../data/sampledFunctions/", instanceName, "-mid_samples-", approx, "_seed-", seed, ".txt"))
        vals <- c()
        for(level in c("high", "low", "mid")){
          if(level == "high"){
            featuresSet <- featuresSetHigh
            featureSetOut <- featureSetOutHigh
            samples <- samplesHigh
          }else if(level == "low"){
            featuresSet <- featuresSetLow
            featureSetOut <- featureSetOutLow
            samples <- samplesLow
          }else if(level == "mid"){
            featuresSet <- featuresSetMid
            featureSetOut <- featureSetOutMid
            samples <- samplesMid
          }
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
            out <- tryCatch({
              localVals <- calculateFeatureSet(feat.object = feat.object, set = set)
              # renamed <- c()
              # for(name in as.character(names(localVals))){
              #   renamed <- c(renamed, paste0("feature_", level, ".", name))
              # }
              # names(localVals) <- renamed
              #print(set)
              allFeatures <- c(allFeatures, localVals)
              
              #colnames(localVals) <- paste0("feature_", level, ".", colnames(localVals))
              #print("TESTING!")
              #return(localVals)
              #allFeatures <- c(allFeatures, calculateFeatureSet(feat.object = feat.object, set = set))
              
            },
            error=function(cond) {return(NA)},
            #warning=function(cond) {print},
            finally={}
            )
            #print(out)
            #if(!is.null(out)){allFeatures <- c(allFeatures, out)}
            #print(out)
            #allFeatures <- c(allFeatures, out)
            #print(out)
            #print(out[[1]])
            #allFeatures <- c(allFeatures, calculateFeatureSet(feat.object = feat.object, set = set))
          }
          #print(allFeatures)
          allFeatures <- allFeatures[featureSetOut]
          renamed <- c()
          for(name in as.character(names(allFeatures))){
            renamed <- c(renamed, paste0("feature_", level, ".", name))
          }
          names(allFeatures) <- renamed
          #print("Ok here")
          #print(allFeatures)
          vals <- c(vals, allFeatures)
          #print(vals)
          #print(paste0("Replaced! Length names ", length(featureSetOut), ", got total of ", length(allFeatures)))
        }
        #print("Got all!")
        # featureNames <- c()
        # for(level in c("high", "low", "mid")){
        #   if(level == "high"){
        #     featuresSet <- featuresSetHigh
        #     featureSetOut <- featureSetOutHigh
        #   }else if(level == "low"){
        #     featuresSet <- featuresSetLow
        #     featureSetOut <- featureSetOutLow
        #   }else if(level == "mid"){
        #     featuresSet <- featuresSetMid
        #     featureSetOut <- featureSetOutMid
        #   }
        #   for(singleFeature in featureSetOut){
        #     featureNames <- c(featureNames, paste0("feature_", level, "-", singleFeature))
        #   }
        # }
        #print("About to save names!")
        #print(class(vals))
        #print(data.frame(t(unlist(vals))))
        #print(vals)
        vals <- data.frame(t(unlist(vals)))
        #print(vals)
        approximationAugmentedData[nrow(approximationAugmentedData), colnames(vals)] <- vals[1,]
        #approximationAugmentedData[nrow(approximationAugmentedData), colnames(approximationAugmentedData)[21:(21+length(featureNames))]] <- vals
        #print("Saved!")
      }
    }
  }
  return(approximationAugmentedData[(nrow(basicData)+1):nrow(approximationAugmentedData), ])
}

calculateFeatureAccuracies <- function(initialData, approximationData, importances, approximations){
  for(approx in approximations){
    # First get all the rows that used this budget
    approxFeatures <- approximationData[str_which(approximationData$instances, paste0("samples-", approx, "_"), negate = FALSE), ]
    importances[, paste0("rrmse_", approx)] <- 0
    for(feat in row.names(importances)){
      vals <- c()
      for(instance in initialData$instances){
        #print(paste0("Working on feature ", feat, " ()"))
        localVals <- approxFeatures[str_which(approxFeatures$instances, instance, negate = FALSE), feat]
        localVals <- localVals[!is.na(localVals)]
        if(length(localVals) == 0){
          print(paste0("Skipping NA from feature ", feat, " in instance ", instance))
          next
        }
        vals <- c(vals, localVals - initialData[initialData$instance == instance, feat])
      }
      rrmse <- sqrt(mean(vals^2))
      if(max(initialData[, feat]) - min(initialData[, feat]) > 0.001){rrmse <- rrmse / (max(initialData[, feat]) - min(initialData[, feat]))}
      importances[feat, paste0("rrmse_", approx)] <- rrmse
    }
  }
  return(importances)
}

# Get processed features of all instances, i..e "real" features
litData <- read.table("../data/instanceSpaceAnalysisInput/litSuiteAllFeatures.csv", header = TRUE, sep = ",")
litData$instances <- as.character(litData$instances)
litData$Source <- as.character(litData$Source)
litData$bestPerformanceWilcoxon <- "None"
litData[litData$algo_Kriging <= 1.05*litData$algo_Co.Kriging &
          litData$algo_Co.Kriging > 1.05*litData$algo_Kriging, "bestPerformanceWilcoxon"] <- "kriging_1_0"
litData[litData$algo_Co.Kriging <= 1.05*litData$algo_Kriging &
          litData$algo_Kriging > 1.05*litData$algo_Co.Kriging, "bestPerformanceWilcoxon"] <- "cokriging_0.8_0.2"

augData <- read.table("../data/instanceSpaceAnalysisInput/augSuiteAllFeatures.csv", header = TRUE, sep = ",")
augData$instances <- as.character(augData$instances)
augData$Source <- as.character(augData$Source)
augData$bestPerformanceWilcoxon <- "None"
augData[augData$algo_Kriging <= 1.05*augData$algo_Co.Kriging &
          augData$algo_Co.Kriging > 1.05*augData$algo_Kriging, "bestPerformanceWilcoxon"] <- "kriging_1_0"
augData[augData$algo_Co.Kriging <= 1.05*augData$algo_Kriging &
          augData$algo_Kriging > 1.05*augData$algo_Co.Kriging, "bestPerformanceWilcoxon"] <- "cokriging_0.8_0.2"




seeds <- 1:20
approxs <- c(5)

# Get approximations
approximations <- storeApproximations(augData, approxs, seeds)
write.table(approximations, paste0("../data/analysis/importanceVSaccuracy/approximations", approxs[[1]], ".csv"), quote = FALSE, row.names = FALSE, sep = ",")
# Fixing column!
# for(approx in c(10, 25, 100)){
#   approximations <- storeApproximations(augData, c(approx), seeds)
#   # Get old data
#   oldApprox <- read.table(paste0("../data/analysis/importanceVSaccuracy/approximations", approx, ".csv"), header = TRUE, sep = ",")
#   # Here should be able to just replace
#   oldApprox[, "feature_CC"] <- approximations$feature_CC
#   write.table(oldApprox, paste0("../data/analysis/importanceVSaccuracy/approximations", approx, ".csv"), quote = FALSE, row.names = FALSE, sep = ",")
# }
# This should get the right values, now read and fix

# Get feature importances
cp <- 0
minPerc <- 0.05
reps <- 100
optReps <- 4
doOpt <- FALSE
techniqueName <- "avoidCokrig"
krigName <- "kriging_1_0"
techniqueTechName <- krigName
features <- colnames(litData)[3:136]
litDataImportances <- performPrediction(litData, techniqueTechName, features, reps, "avoidCokrig-litData-expandedFeatures", minPerc, optReps, cp = cp, doOpt = doOpt)

features <- colnames(augData)[3:136]
augDataImportances <- performPrediction(rbind(litData, augData), techniqueTechName, features, reps, "avoidCokrig-augData-expandedFeatures", minPerc, optReps, cp = cp, doOpt = doOpt)

# Now get accuracies in approximating different features
litImportancesAndAccuracies <- calculateFeatureAccuracies(litData, approximations, litDataImportances, approxs)
augImportancesAndAccuracies <- calculateFeatureAccuracies(augData, approximations, augDataImportances, approxs)

# Finally should be able to plot!
# Seem like I want same column but with different labels, ugh
data <- litImportancesAndAccuracies
data$approx_budget <- 0
combinedImportances <- data.frame("feature" = character(0), "importance" = numeric(0), "rrmse" = numeric(0), "approx_budget" = integer(0))
for(approx in approxs){
  temp <- data[, c("importance", paste0("rrmse_", approx), "approx_budget")]
  temp$feature <- row.names(temp)
  row.names(temp) <- NULL
  temp$approx_budget <- approx
  colnames(temp) <- c("importance", "rrmse", "approx_budget", "feature")
  temp <- temp[, c("feature", "importance", "rrmse", "approx_budget")]
  combinedImportances <- rbind(combinedImportances, temp)
  # combinedImportances[(nrow(combinedImportances)+1):(nrow(combinedImportances)+nrow(data)), ] <-
  #   data[, c("importance", paste0("rrmse_", approx), "approx_budget")]
  # combinedImportances[(nrow(combinedImportances) - nrow(data) + 1):nrow(combinedImportances), "approx_budget"] <- approx
}
combinedImportancesLit <- combinedImportances
write.table(combinedImportancesLit, paste0("../data/analysis/importanceVSaccuracy/combinedImportancesLit", approxs[[1]], ".csv"), quote = FALSE, row.names = FALSE, sep = ",")
# ggplot(combinedImportances, aes(x = importance, y = rrmse)) +
#   geom_point(aes(color = factor(approx_budget)))
# ggsave(paste0("../data/analysis/importanceVSaccuracy/litData.png"), width = 16, height = 9, units = "cm")


data <- augImportancesAndAccuracies
data$approx_budget <- 0
combinedImportances <- data.frame( "importance" = numeric(0), "rrmse" = numeric(0), "approx_budget" = integer(0))
for(approx in approxs){
  temp <- data[, c("importance", paste0("rrmse_", approx), "approx_budget")]
  temp$feature <- row.names(temp)
  row.names(temp) <- NULL
  temp$approx_budget <- approx
  colnames(temp) <- c("importance", "rrmse", "approx_budget", "feature")
  temp <- temp[, c("feature", "importance", "rrmse", "approx_budget")]
  combinedImportances <- rbind(combinedImportances, temp)
}
combinedImportancesAug <- combinedImportances
write.table(combinedImportancesAug, paste0("../data/analysis/importanceVSaccuracy/combinedImportancesAug", approxs[[1]], ".csv"), quote = FALSE, row.names = FALSE, sep = ",")
# ggplot(combinedImportances, aes(x = importance, y = rrmse)) +
#   geom_point(aes(color = factor(approx_budget)))
# ggsave(paste0("../data/analysis/importanceVSaccuracy/augData.png"), width = 16, height = 9, units = "cm")



data <- read.table("../data/analysis/importanceVSaccuracy/combinedImportancesLit5.csv", header = TRUE, sep = ",")
for(approx in c(10, 25, 100)){
  data <- rbind(data, read.table(paste0("../data/analysis/importanceVSaccuracy/combinedImportancesLit", approx, ".csv"), header = TRUE, sep = ","))
}
ggplot(data, aes(x = importance, y = rrmse)) +
  geom_point(aes(color = factor(approx_budget))) +
  ylim(0, 2)
ggsave(paste0("../data/analysis/importanceVSaccuracy/litData.png"), width = 16, height = 9, units = "cm")

data <- read.table("../data/analysis/importanceVSaccuracy/combinedImportancesAug5.csv", header = TRUE, sep = ",")
for(approx in c(10, 25, 100)){
  data <- rbind(data, read.table(paste0("../data/analysis/importanceVSaccuracy/combinedImportancesAug", approx, ".csv"), header = TRUE, sep = ","))
}
ggplot(data, aes(x = importance, y = rrmse)) +
  geom_point(aes(color = factor(approx_budget))) +
  ylim(0, 2)
ggsave(paste0("../data/analysis/importanceVSaccuracy/augData.png"), width = 16, height = 9, units = "cm")

# SAVE IT ALL TO AVOID REDOING HOURS OF WORK, COMMENTED OUT TO NOT OVERRIDE BY MISTAKE
#write.table(approximations, "../data/analysis/importanceVSaccuracy/approximations.csv", quote = FALSE, row.names = FALSE, sep = ",")
#write.table(combinedImportancesAug, "../data/analysis/importanceVSaccuracy/combinedImportancesAug.csv", quote = FALSE, row.names = FALSE, sep = ",")
#write.table(combinedImportancesLit, "../data/analysis/importanceVSaccuracy/combinedImportancesLit.csv", quote = FALSE, row.names = FALSE, sep = ",")
