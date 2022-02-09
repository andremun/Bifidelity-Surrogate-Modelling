# Ok first of all want to get all the necessary data, for now work with the lit data
source("dataProcessor.R")
library(tidyft)

analyseChanges <- function(surrogateModelFitData, surrogateModelExplorationData, surrogateModelExploitationData, functionsAndFeatures, plotPrefix, mult = 1){
  lwd <- 2
  cex <- 1
  
  functionsAndFeatures <- functionsAndFeatures[, c("instance", "fMin", "fMax")]
  for(instance in functionsAndFeatures$instance){
    # Get subset data from the three datasets based on this instance
    subset01 <- surrogateModelFitData[str_which(surrogateModelFitData$instance, paste0("_", instance), negate = FALSE),]
    subset02 <- surrogateModelExplorationData[str_which(surrogateModelExplorationData$instance, paste0("_", instance), negate = FALSE),]
    subset03 <- surrogateModelExploitationData[str_which(surrogateModelExploitationData$instance, paste0("_", instance), negate = FALSE),]
    
    # Now should be able to expand on the dataframe
    if(!is.null(surrogateModelFitData) && nrow(subset01) == 1){
      functionsAndFeatures[functionsAndFeatures$instance == instance, "krigSurrogateModelFit"] <- 
        subset01$`kriging_1_0-mean`/(functionsAndFeatures[functionsAndFeatures$instance == instance, "fMax"] - functionsAndFeatures[functionsAndFeatures$instance == instance, "fMin"])
      
      functionsAndFeatures[functionsAndFeatures$instance == instance, "cokrigSurrogateModelFit"] <- 
        subset01$`cokriging_0.8_0.2-mean`/(functionsAndFeatures[functionsAndFeatures$instance == instance, "fMax"] - functionsAndFeatures[functionsAndFeatures$instance == instance, "fMin"])
      
      if(subset01$bestPerformanceWilcoxon == "None"){
        functionsAndFeatures[functionsAndFeatures$instance == instance, "surrogateModelFitBest"] <- "None"
      }else if(subset01$bestPerformanceWilcoxon == "cokriging_0.8_0.2"){
        functionsAndFeatures[functionsAndFeatures$instance == instance, "surrogateModelFitBest"] <- "cokriging"
      }else if(subset01$bestPerformanceWilcoxon == "kriging_1_0"){
        functionsAndFeatures[functionsAndFeatures$instance == instance, "surrogateModelFitBest"] <- "kriging"
      }else{
        functionsAndFeatures[functionsAndFeatures$instance == instance, "surrogateModelFitBest"] <- "Weitd!!"
      }
    }else{
      #print(paste0("Skipping ", instance, " as no surrogate model fit data"))
    }
    
  
    if(!is.null(surrogateModelExplorationData) && nrow(subset02) == 1){
      # Now onto surrogate model exploration
      functionsAndFeatures[functionsAndFeatures$instance == instance, "krigSurrogateModelExploration"] <- 
        subset02$`kriging_0.5_0-mean`/(functionsAndFeatures[functionsAndFeatures$instance == instance, "fMax"] - functionsAndFeatures[functionsAndFeatures$instance == instance, "fMin"])
      
      functionsAndFeatures[functionsAndFeatures$instance == instance, "cokrigSurrogateModelExploration"] <- 
        subset02$`cokriging_0.4_0.1-mean`/(functionsAndFeatures[functionsAndFeatures$instance == instance, "fMax"] - functionsAndFeatures[functionsAndFeatures$instance == instance, "fMin"])
      
      if(subset02$bestPerformanceWilcoxon == "None"){
        functionsAndFeatures[functionsAndFeatures$instance == instance, "SurrogateModelExplorationBest"] <- "None"
      }else if(subset02$bestPerformanceWilcoxon == "cokriging_0.4_0.1"){
        functionsAndFeatures[functionsAndFeatures$instance == instance, "SurrogateModelExplorationBest"] <- "cokriging"
      }else if(subset02$bestPerformanceWilcoxon == "kriging_0.5_0"){
        functionsAndFeatures[functionsAndFeatures$instance == instance, "SurrogateModelExplorationBest"] <- "kriging"
      }else{
        functionsAndFeatures[functionsAndFeatures$instance == instance, "SurrogateModelExplorationBest"] <- "Weitd!!"
      }
      
    }else{
      #print(paste0("Skipping ", instance, " as no surrogate model exploration"))
    }
    
    # Now function optimisation
    if(!is.null(surrogateModelExploitationData) && nrow(subset03) == 1){
      functionsAndFeatures[functionsAndFeatures$instance == instance, "krigSurrogateModelExploitation"] <- 
        (subset03$`kriging_0.5_0-mean` - functionsAndFeatures[functionsAndFeatures$instance == instance, "fMin"])/(functionsAndFeatures[functionsAndFeatures$instance == instance, "fMax"] - functionsAndFeatures[functionsAndFeatures$instance == instance, "fMin"])
      
      functionsAndFeatures[functionsAndFeatures$instance == instance, "cokrigSurrogateModelExploitation"] <- 
        (subset03$`cokriging_0.4_0.1-mean` - functionsAndFeatures[functionsAndFeatures$instance == instance, "fMin"])/(functionsAndFeatures[functionsAndFeatures$instance == instance, "fMax"] - functionsAndFeatures[functionsAndFeatures$instance == instance, "fMin"])
      
      if(subset03$bestPerformanceWilcoxon == "None"){
        functionsAndFeatures[functionsAndFeatures$instance == instance, "SurrogateModelExploitationBest"] <- "None"
      }else if(subset03$bestPerformanceWilcoxon == "cokriging_0.4_0.1"){
        functionsAndFeatures[functionsAndFeatures$instance == instance, "SurrogateModelExploitationBest"] <- "cokriging"
      }else if(subset03$bestPerformanceWilcoxon == "kriging_0.5_0"){
        functionsAndFeatures[functionsAndFeatures$instance == instance, "SurrogateModelExploitationBest"] <- "kriging"
      }else{
        functionsAndFeatures[functionsAndFeatures$instance == instance, "SurrogateModelExplorationBest"] <- "Weitd!!"
      }
    }else{
      #print(paste0("Skipping ", instance, " as no surrogate model exploitation"))
    }
  }
  print(paste0("Got ", nrow(functionsAndFeatures[!is.na(functionsAndFeatures$surrogateModelFitBest), ]), " instances"))
  if(nrow(functionsAndFeatures[!is.na(functionsAndFeatures$surrogateModelFitBest) & !is.na(functionsAndFeatures$SurrogateModelExplorationBest), ]) > 0){
    data <- functionsAndFeatures[!is.na(functionsAndFeatures$surrogateModelFitBest) & !is.na(functionsAndFeatures$SurrogateModelExplorationBest), ]
    print(paste0("Now printing percentages from Surrogate Model Fit to Surrogate Model Exploration, have ", 
                 nrow(functionsAndFeatures[!is.na(functionsAndFeatures$surrogateModelFitBest) & !is.na(functionsAndFeatures$SurrogateModelExplorationBest), ]),
                 " instances"))
    print(paste0("Krig to Krig: ", 100*nrow(data[data$surrogateModelFitBest == "kriging" & data$SurrogateModelExplorationBest == "kriging", ])/nrow(data), "%"))
    print(paste0("Krig to None: ", 100*nrow(data[data$surrogateModelFitBest == "kriging" & data$SurrogateModelExplorationBest == "None", ])/nrow(data), "%"))
    print(paste0("Krig to Cokrig: ", 100*nrow(data[data$surrogateModelFitBest == "kriging" & data$SurrogateModelExplorationBest == "cokriging", ])/nrow(data), "%"))
    print(paste0("None to Krig: ", 100*nrow(data[data$surrogateModelFitBest == "None" & data$SurrogateModelExplorationBest == "kriging", ])/nrow(data), "%"))
    print(paste0("None to None: ", 100*nrow(data[data$surrogateModelFitBest == "None" & data$SurrogateModelExplorationBest == "None", ])/nrow(data), "%"))
    print(paste0("None to Cokrig: ", 100*nrow(data[data$surrogateModelFitBest == "None" & data$SurrogateModelExplorationBest == "cokriging", ])/nrow(data), "%"))
    print(paste0("Cokrig to Krig: ", 100*nrow(data[data$surrogateModelFitBest == "cokriging" & data$SurrogateModelExplorationBest == "kriging", ])/nrow(data), "%"))
    print(paste0("Cokrig to None: ", 100*nrow(data[data$surrogateModelFitBest == "cokriging" & data$SurrogateModelExplorationBest == "None", ])/nrow(data), "%"))
    print(paste0("Cokrig to Cokrig: ", 100*nrow(data[data$surrogateModelFitBest == "cokriging" & data$SurrogateModelExplorationBest == "cokriging", ])/nrow(data), "%"))
    
    # Now do plot
    png(paste0("../data/analysis/comparativePerformance/", plotPrefix, "-01-surrogateModelFit-surrogateModelExploration-krig"), width = mult*682, height = mult*682)
    plot(c(), c(),
         col = "black",
         type = "p",
         ylab = "Surrogate Model Fit",
         xlab = "Surrogate Model Exploration",
         ylim = c(-0.1, 1.1*max(data$krigSurrogateModelFit)),
         xlim = c(-0.1, 1.1*max(data$krigSurrogateModelExploration)),
         # xaxt = "n",
         pch = 20)
    legs <- c()
    cols <- c()
    bgs <- c()
    for(firstIndex in 1:3){
      for(secondIndex in 1:3){
        firstPerf <- c("kriging", "None", "cokriging")[[firstIndex]]
        secondPerf <- c("kriging", "None", "cokriging")[[secondIndex]]
        firstCol <- c("red", "green", "blue")[[firstIndex]]
        secondCol <- c("red", "green", "blue")[[secondIndex]]
        legs <- c(legs, paste0(firstPerf, "->", secondPerf))
        cols <- c(cols, firstCol)
        bgs <- c(bgs, secondCol)
        points(data[data$surrogateModelFitBest == firstPerf & data$SurrogateModelExplorationBest == secondPerf, "krigSurrogateModelExploration"],
               data[data$surrogateModelFitBest == firstPerf & data$SurrogateModelExplorationBest == secondPerf, "krigSurrogateModelFit"],
               pch=21, col=firstCol, bg=secondCol, lwd=lwd, cex = cex)
      }
    }
    legend("topleft", legend = legs, pch = 21, lwd = lwd, col = cols, pt.bg=bgs, pt.cex = cex)
    out <- dev.off()
    
    png(paste0("../data/analysis/comparativePerformance/", plotPrefix, "-01-surrogateModelFit-surrogateModelExploration-cokrig"), width = mult*682, height = mult*682)
    plot(c(), c(),
         col = "black",
         type = "p",
         ylab = "Surrogate Model Fit",
         xlab = "Surrogate Model Exploration",
         ylim = c(-0.1, 1.1*max(data$cokrigSurrogateModelFit)),
         xlim = c(-0.1, 1.1*max(data$cokrigSurrogateModelExploration)),
         # xaxt = "n",
         pch = 20)
    legs <- c()
    cols <- c()
    bgs <- c()
    for(firstIndex in 1:3){
      for(secondIndex in 1:3){
        firstPerf <- c("kriging", "None", "cokriging")[[firstIndex]]
        secondPerf <- c("kriging", "None", "cokriging")[[secondIndex]]
        firstCol <- c("red", "green", "blue")[[firstIndex]]
        secondCol <- c("red", "green", "blue")[[secondIndex]]
        legs <- c(legs, paste0(firstPerf, "->", secondPerf))
        cols <- c(cols, firstCol)
        bgs <- c(bgs, secondCol)
        points(data[data$surrogateModelFitBest == firstPerf & data$SurrogateModelExplorationBest == secondPerf, "cokrigSurrogateModelExploration"],
               data[data$surrogateModelFitBest == firstPerf & data$SurrogateModelExplorationBest == secondPerf, "cokrigSurrogateModelFit"],
               pch=21, col=firstCol, bg=secondCol, lwd=lwd, cex = cex)
      }
    }
    legend("topleft", legend = legs, pch = 21, lwd = lwd, col = cols, pt.bg=bgs, pt.cex = cex)
    out <- dev.off()
  }
  
  
  
  
  if(nrow(functionsAndFeatures[!is.na(functionsAndFeatures$surrogateModelFitBest) & !is.na(functionsAndFeatures$SurrogateModelExploitationBest), ]) > 0){
    data <- functionsAndFeatures[!is.na(functionsAndFeatures$surrogateModelFitBest) & !is.na(functionsAndFeatures$SurrogateModelExploitationBest), ]
    print(paste0("Now printing percentages from Surrogate Model Fit to Surrogate Model Exploitation, have ", 
          nrow(functionsAndFeatures[!is.na(functionsAndFeatures$surrogateModelFitBest) & !is.na(functionsAndFeatures$SurrogateModelExploitationBest), ]),
          " instances"))
    print(paste0("Krig to Krig: ", 100*nrow(data[data$surrogateModelFitBest == "kriging" & data$SurrogateModelExploitationBest == "kriging", ])/nrow(data), "%"))
    print(paste0("Krig to None: ", 100*nrow(data[data$surrogateModelFitBest == "kriging" & data$SurrogateModelExploitationBest == "None", ])/nrow(data), "%"))
    print(paste0("Krig to Cokrig: ", 100*nrow(data[data$surrogateModelFitBest == "kriging" & data$SurrogateModelExploitationBest == "cokriging", ])/nrow(data), "%"))
    print(paste0("None to Krig: ", 100*nrow(data[data$surrogateModelFitBest == "None" & data$SurrogateModelExploitationBest == "kriging", ])/nrow(data), "%"))
    print(paste0("None to None: ", 100*nrow(data[data$surrogateModelFitBest == "None" & data$SurrogateModelExploitationBest == "None", ])/nrow(data), "%"))
    print(paste0("None to Cokrig: ", 100*nrow(data[data$surrogateModelFitBest == "None" & data$SurrogateModelExploitationBest == "cokriging", ])/nrow(data), "%"))
    print(paste0("Cokrig to Krig: ", 100*nrow(data[data$surrogateModelFitBest == "cokriging" & data$SurrogateModelExploitationBest == "kriging", ])/nrow(data), "%"))
    print(paste0("Cokrig to None: ", 100*nrow(data[data$surrogateModelFitBest == "cokriging" & data$SurrogateModelExploitationBest == "None", ])/nrow(data), "%"))
    print(paste0("Cokrig to Cokrig: ", 100*nrow(data[data$surrogateModelFitBest == "cokriging" & data$SurrogateModelExploitationBest == "cokriging", ])/nrow(data), "%"))
    
    png(paste0("../data/analysis/comparativePerformance/", plotPrefix, "-02-surrogateModelFit-surrogateModelExploitation-krig"), width = mult*682, height = mult*682)
    plot(c(), c(),
         col = "black",
         type = "p",
         ylab = "Surrogate Model Fit",
         xlab = "Surrogate Model Exploitation",
         ylim = c(-0.1, 1.1*max(data$krigSurrogateModelFit)),
         xlim = c(-0.1, 1.1*max(data$krigSurrogateModelExploitation)),
         # xaxt = "n",
         pch = 20)
    legs <- c()
    cols <- c()
    bgs <- c()
    for(firstIndex in 1:3){
      for(secondIndex in 1:3){
        firstPerf <- c("kriging", "None", "cokriging")[[firstIndex]]
        secondPerf <- c("kriging", "None", "cokriging")[[secondIndex]]
        firstCol <- c("red", "green", "blue")[[firstIndex]]
        secondCol <- c("red", "green", "blue")[[secondIndex]]
        legs <- c(legs, paste0(firstPerf, "->", secondPerf))
        cols <- c(cols, firstCol)
        bgs <- c(bgs, secondCol)
        points(data[data$surrogateModelFitBest == firstPerf & data$SurrogateModelExploitation == secondPerf, "krigSurrogateModelExploitation"],
               data[data$surrogateModelFitBest == firstPerf & data$SurrogateModelExploitation == secondPerf, "krigSurrogateModelFit"],
               pch=21, col=firstCol, bg=secondCol, lwd=lwd, cex = cex)
      }
    }
    legend("topleft", legend = legs, pch = 21, lwd = lwd, col = cols, pt.bg=bgs, pt.cex = cex)
    out <- dev.off()
    
    png(paste0("../data/analysis/comparativePerformance/", plotPrefix, "-02-surrogateModelFit-surrogateModelExploitation-cokrig"), width = mult*682, height = mult*682)
    plot(c(), c(),
         col = "black",
         type = "p",
         ylab = "Surrogate Model Fit",
         xlab = "Surrogate Model Exploitation",
         ylim = c(-0.1, 1.1*max(data$cokrigSurrogateModelFit)),
         xlim = c(-0.1, 1.1*max(data$cokrigSurrogateModelExploitation)),
         # xaxt = "n",
         pch = 20)
    legs <- c()
    cols <- c()
    bgs <- c()
    for(firstIndex in 1:3){
      for(secondIndex in 1:3){
        firstPerf <- c("kriging", "None", "cokriging")[[firstIndex]]
        secondPerf <- c("kriging", "None", "cokriging")[[secondIndex]]
        firstCol <- c("red", "green", "blue")[[firstIndex]]
        secondCol <- c("red", "green", "blue")[[secondIndex]]
        legs <- c(legs, paste0(firstPerf, "->", secondPerf))
        cols <- c(cols, firstCol)
        bgs <- c(bgs, secondCol)
        points(data[data$surrogateModelFitBest == firstPerf & data$SurrogateModelExploitation == secondPerf, "cokrigSurrogateModelExploitation"],
               data[data$surrogateModelFitBest == firstPerf & data$SurrogateModelExploitation == secondPerf, "cokrigSurrogateModelFit"],
               pch=21, col=firstCol, bg=secondCol, lwd=lwd, cex = cex)
      }
    }
    legend("topleft", legend = legs, pch = 21, lwd = lwd, col = cols, pt.bg=bgs, pt.cex = cex)
    out <- dev.off()
  }
  
  if(nrow(functionsAndFeatures[!is.na(functionsAndFeatures$SurrogateModelExplorationBest) & !is.na(functionsAndFeatures$SurrogateModelExploitationBest), ]) > 0){
    data <- functionsAndFeatures[!is.na(functionsAndFeatures$SurrogateModelExplorationBest) & !is.na(functionsAndFeatures$SurrogateModelExploitationBest), ]
    print(paste0("Now printing percentages from Surrogate Model Exploration to Surrogate Model Exploitation, have ", 
          nrow(functionsAndFeatures[!is.na(functionsAndFeatures$SurrogateModelExplorationBest) & !is.na(functionsAndFeatures$SurrogateModelExploitationBest), ]),
          " instances"))
    print(paste0("Krig to Krig: ", 100*nrow(data[data$SurrogateModelExplorationBest == "kriging" & data$SurrogateModelExploitationBest == "kriging", ])/nrow(data), "%"))
    print(paste0("Krig to None: ", 100*nrow(data[data$SurrogateModelExplorationBest == "kriging" & data$SurrogateModelExploitationBest == "None", ])/nrow(data), "%"))
    print(paste0("Krig to Cokrig: ", 100*nrow(data[data$SurrogateModelExplorationBest == "kriging" & data$SurrogateModelExploitationBest == "cokriging", ])/nrow(data), "%"))
    print(paste0("None to Krig: ", 100*nrow(data[data$SurrogateModelExplorationBest == "None" & data$SurrogateModelExploitationBest == "kriging", ])/nrow(data), "%"))
    print(paste0("None to None: ", 100*nrow(data[data$SurrogateModelExplorationBest == "None" & data$SurrogateModelExploitationBest == "None", ])/nrow(data), "%"))
    print(paste0("None to Cokrig: ", 100*nrow(data[data$SurrogateModelExplorationBest == "None" & data$SurrogateModelExploitationBest == "cokriging", ])/nrow(data), "%"))
    print(paste0("Cokrig to Krig: ", 100*nrow(data[data$SurrogateModelExplorationBest == "cokriging" & data$SurrogateModelExploitationBest == "kriging", ])/nrow(data), "%"))
    print(paste0("Cokrig to None: ", 100*nrow(data[data$SurrogateModelExplorationBest == "cokriging" & data$SurrogateModelExploitationBest == "None", ])/nrow(data), "%"))
    print(paste0("Cokrig to Cokrig: ", 100*nrow(data[data$SurrogateModelExplorationBest == "cokriging" & data$SurrogateModelExploitationBest == "cokriging", ])/nrow(data), "%"))
    
    png(paste0("../data/analysis/comparativePerformance/", plotPrefix, "-03-surrogateModelExploration-surrogateModelExploitation-krig"), width = mult*682, height = mult*682)
    plot(c(), c(),
         col = "black",
         type = "p",
         ylab = "Surrogate Model Exploration",
         xlab = "Surrogate Model Exploitation",
         ylim = c(-0.1, 1.1*max(data$krigSurrogateModelExploration)),
         xlim = c(-0.1, 1.1*max(data$krigSurrogateModelExploitation)),
         # xaxt = "n",
         pch = 20)
    legs <- c()
    cols <- c()
    bgs <- c()
    for(firstIndex in 1:3){
      for(secondIndex in 1:3){
        firstPerf <- c("kriging", "None", "cokriging")[[firstIndex]]
        secondPerf <- c("kriging", "None", "cokriging")[[secondIndex]]
        firstCol <- c("red", "green", "blue")[[firstIndex]]
        secondCol <- c("red", "green", "blue")[[secondIndex]]
        legs <- c(legs, paste0(firstPerf, "->", secondPerf))
        cols <- c(cols, firstCol)
        bgs <- c(bgs, secondCol)
        points(data[data$SurrogateModelExplorationBest == firstPerf & data$SurrogateModelExploitation == secondPerf, "krigSurrogateModelExploitation"],
               data[data$SurrogateModelExplorationBest == firstPerf & data$SurrogateModelExploitation == secondPerf, "krigSurrogateModelExploration"],
               pch=21, col=firstCol, bg=secondCol, lwd=lwd, cex = cex)
      }
    }
    legend("topleft", legend = legs, pch = 21, lwd = lwd, col = cols, pt.bg=bgs, pt.cex = cex)
    out <- dev.off()
    
    png(paste0("../data/analysis/comparativePerformance/", plotPrefix, "-03-surrogateModelExploration-surrogateModelExploitation-cokrig"), width = mult*682, height = mult*682)
    plot(c(), c(),
         col = "black",
         type = "p",
         ylab = "Surrogate Model Exploration",
         xlab = "Surrogate Model Exploitation",
         ylim = c(-0.1, 1.1*max(data$cokrigSurrogateModelExploration)),
         xlim = c(-0.1, 1.1*max(data$cokrigSurrogateModelExploitation)),
         # xaxt = "n",
         pch = 20)
    legs <- c()
    cols <- c()
    bgs <- c()
    for(firstIndex in 1:3){
      for(secondIndex in 1:3){
        firstPerf <- c("kriging", "None", "cokriging")[[firstIndex]]
        secondPerf <- c("kriging", "None", "cokriging")[[secondIndex]]
        firstCol <- c("red", "green", "blue")[[firstIndex]]
        secondCol <- c("red", "green", "blue")[[secondIndex]]
        legs <- c(legs, paste0(firstPerf, "->", secondPerf))
        cols <- c(cols, firstCol)
        bgs <- c(bgs, secondCol)
        points(data[data$SurrogateModelExplorationBest == firstPerf & data$SurrogateModelExploitation == secondPerf, "cokrigSurrogateModelExploitation"],
               data[data$SurrogateModelExplorationBest == firstPerf & data$SurrogateModelExploitation == secondPerf, "cokrigSurrogateModelExploration"],
               pch=21, col=firstCol, bg=secondCol, lwd=lwd, cex = cex)
      }
    }
    legend("topleft", legend = legs, pch = 21, lwd = lwd, col = cols, pt.bg=bgs, pt.cex = cex)
    out <- dev.off()
  }
  
  return(functionsAndFeatures)
  
}

# dataset01_litSuiteSurrogateModelFit <- combineAndProcessResults("01-litSuiteSurrogateModelFit", 1, 906)
# dataset02a_litSuiteSurrogateModelExploration <- combineAndProcessResults("02a-litSuiteSurrogateModelExploration", 1, 686)
# dataset03a_litSuiteSurrogateModelExploitation <- combineAndProcessResults("03a-litSuiteSurrogateModelExploitation", 1, 686)
# dataset04_modelAccuracySuiteSurrogateModelFit <- combineAndProcessResults("04-modelAccuracySuiteSurrogateModelFit", 1, 1098, 6)
# dataset05a_modelAccuracySuiteSurrogateModelExploration <- combineAndProcessResults("05a-modelAccuracySuiteSurrogateModelExploration", 1, 4208)
# #dataset06a_modelAccuracySuiteSurrogateModelExploitation <- combineAndProcessResults("06a-modelAccuracySuiteSurrogateModelExploitation", 1, 4208)
# dataset07_functionOptSuiteSurrogateModelFit <- combineAndProcessResults("07-functionOptSuiteSurrogateModelFit", 1, 1083, 3)
# #dataset08a_functionOptSuiteSurrogateModelExploration <- combineAndProcessResults("08a-functionOptSuiteSurrogateModelExploration", 1, 2102)
# dataset09a_functionOptSuiteSurrogateModelExploitation <- combineAndProcessResults("09a-functionOptSuiteSurrogateModelExploitation", 1, 2102)

functionsAndFeatures <- read.table("../data/clusterResults/allFunctionsFeatures_arrayJob1-2266.txt", header = TRUE, sep = " ", fill = TRUE)
functionsAndFeatures <- functionsAndFeatures[str_which(functionsAndFeatures$instance, "function12", negate = TRUE), ]
val1 <- analyseChanges(dataset01_litSuiteSurrogateModelFit,
               dataset02a_litSuiteSurrogateModelExploration,
               dataset03a_litSuiteSurrogateModelExploitation,
               functionsAndFeatures,
               "01-litSuite", mult = 1)

val2 <- analyseChanges(rbind(dataset01_litSuiteSurrogateModelFit, dataset04_modelAccuracySuiteSurrogateModelFit),
               rbind(dataset02a_litSuiteSurrogateModelExploration, dataset05a_modelAccuracySuiteSurrogateModelExploration),
               NULL,
               functionsAndFeatures,
               "02-modelAccuracySuite", mult = 1)

val3 <- analyseChanges(rbind(dataset01_litSuiteSurrogateModelFit, dataset07_functionOptSuiteSurrogateModelFit),
               NULL,
               rbind(dataset03a_litSuiteSurrogateModelExploitation, dataset09a_functionOptSuiteSurrogateModelExploitation),
               functionsAndFeatures,
               "03-functionOptSuite", mult = 1)



interest <- dataset09a_functionOptSuiteSurrogateModelExploitation[str_which(dataset09a_functionOptSuiteSurrogateModelExploitation$instance, "function12", negate = FALSE), 
                                                                  c("instance", "cokriging_0.4_0.1-mean", "kriging_0.5_0-mean")]

