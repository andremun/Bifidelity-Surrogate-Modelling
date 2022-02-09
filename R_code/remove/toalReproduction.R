source("dataProcessor.R")

# All data should be in the same place now! Just get average of data and good to go!
performance1 <- combineAndProcessResultsWithoutKriging("toalReproductionSetup", 1, 848)
performance2 <- combineAndProcessResultsWithoutCoKriging("toalReproductionSetup", 1, 848)

performance1 <- performance1[, c("instance", "bestMean", "globalCorrelation_R2", "relativeError")]
performance2 <- performance2[, c("instance", "bestMean", "globalCorrelation_R2", "relativeError")]
performance1$isKrig = FALSE
performance2$isKrig = TRUE
performance <- rbind(performance1, performance2)

names <- c("ToalBranin", "ToalPaciorek", "ToalHartmannH3", "ToalTrid")

for(i in 1:4){
  if(i == 1 | i == 2){num <- 10}
  else if(i == 3){num <- 15}
  else{num <- 50}
  krigVal <- performance[performance$instance == paste0("modelAccuracy_", num, "_0.1_", names[[i]], "0.00") & 
                           performance$isKrig, "bestMean"]
  aValsName <- c("0.00", "0.05", "0.10", "0.15", "0.20", "0.25", "0.30", "0.35", "0.40", "0.45", "0.50", "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85", "0.90", "0.95", "1.00")
  aVals <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0)
  
  vals4 <- c()
  vals7 <- c()
  vals10 <- c()
  vals15 <- c()
  vals20 <- c()
  r2 <- c()
  error <- c()
  for(a in aValsName){
    vals4 <- c(vals4, performance[performance$instance == paste0("modelAccuracy_", num, "_0.25_", names[[i]], a) &
                                    !performance$isKrig, "bestMean"])

    vals7 <- c(vals7, performance[performance$instance == paste0("modelAccuracy_", num, "_0.14_", names[[i]], a) &
                                    !performance$isKrig, "bestMean"])
    
    vals10 <- c(vals10, performance[performance$instance == paste0("modelAccuracy_", num, "_0.1_", names[[i]], a) &
                                      !performance$isKrig, "bestMean"])
    
    vals15 <- c(vals15, performance[performance$instance == paste0("modelAccuracy_", num, "_0.06_", names[[i]], a) &
                                      !performance$isKrig, "bestMean"])
    
    vals20 <- c(vals20, performance[performance$instance == paste0("modelAccuracy_", num, "_0.05_", names[[i]], a) &
                                      !performance$isKrig, "bestMean"])
    
    r2 <- c(r2, performance[performance$instance == paste0("modelAccuracy_", num, "_0.05_", names[[i]], a) &
                              !performance$isKrig, "globalCorrelation_R2"])
    
    error <- c(error, performance[performance$instance == paste0("modelAccuracy_", num, "_0.05_", names[[i]], a) &
                                    !performance$isKrig, "relativeError"])
  }
  collated <- c(vals4, vals7, vals10, vals15, vals20)
  # print("Vals 4")
  # print(vals4)
  # print("Vals 7")
  # print(vals7)
  # print("Vals 10")
  # print(vals10)
  # print("Vals 15")
  # print(vals15)
  # print("Vals 20")
  # print(vals20)
  # print("r2")
  # print(r2)
  # print("err")
  # print(error)
  # Time to print!
  png(paste0("../data/analysis/toalResultsReplicated/", names[[i]], "-performance.png"), width = 682, height = 682)
  plot(c(), c(),
       col = "black",
       type = "p",
       ylab = "model error",
       xlab = "A",
       #ylim = c(0.9 * min(comparisonPerformance$allPointsDownComparitivePerformance), 1.1 * max(comparisonPerformance$allPointsDownComparitivePerformance)),
       ylim = c(0.9*min(collated), 1.1*max(collated)),
       xlim = c(0, 1),
       # xaxt = "n",
       pch = 20)
  
  points(aVals, vals4, pch = 1, col = "black")
  points(aVals, vals7, pch = 4, col = "black")
  points(aVals, vals10, pch = 8, col = "black")
  points(aVals, vals15, pch = 5, col = "black")
  if(names[[i]] == "ToalHartmannH3" || names[[i]] == "ToalTrid"){points(aVals, vals20, pch = 0, col = "black")}
  lines(c(0, 1), c(krigVal, krigVal), lt = "dashed")
  if(names[[i]] == "ToalHartmannH3" || names[[i]] == "ToalTrid"){legend("topleft", legend = c("4:1", "7:1", "10:1", "15:1", "20:1"), pch = c(1, 4, 8, 5, 0), col = "black")}
  else{legend("topleft", legend = c("4:1", "7:1", "10:1", "15:1"), pch = c(1, 4, 8, 5), col = "black")}
  out <- dev.off()

  # Now plot the correlation plot
  png(paste0("../data/analysis/toalResultsReplicated/", names[[i]], "-correlation.png"), width = 682, height = 682)
  plot(c(), c(),
       col = "black",
       type = "p",
       ylab = "r2",
       xlab = "A",
       ylim = c(0, 1),
       xlim = c(0, 1),
       # xaxt = "n",
       pch = 20)
  lines(aVals, r2, type = "b", lt = "dashed")
  lines(aVals, error, type = "b", lt = "dashed")
  
  out <- dev.off()
}






