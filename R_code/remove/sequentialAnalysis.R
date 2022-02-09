source("arrayJobCombiner.R")

experimentalExpectedValue <- function(vals, probs){
  total <- 0
  for(i in 1:length(vals)){
    total <- total + vals[[i]] * probs[[i]]
  }
  return(total)
}

experimentalVariance <- function(vals, probs, mean){
  total <- 0
  for(i in 1:length(vals)){
    total <- total + vals[[i]] * vals[[i]] * probs[[i]]
  }
  return(total - mean * mean)
}


runName <- "2DsequentialKrigSetup"
averageData <- getAveragePerformance(runName, 1, 700)
data <- combineArrayResults(runName, 1, 700)
percentages <- c(0.5, 0.25, 0.1, 0.05, 0.01, 0.005, 0.001)

instance <- c()
budget <- c()
bestInitialBudget <- c()
meanBest <- c()
deviationBest <- c()

functions <- unique(data$instance)
budgets <-unique(data$budget)

for(func in functions){
  for(budg in budgets){
    instance <- c(instance, func)
    budget <- c(budget, budg)
    # Extract rows
    tempData <- averageData[averageData$instance == func & averageData$budget == budg, ]
    # Get best performing
    bestInitialBudget <- c(bestInitialBudget, tempData$initialBudgetHigh[tempData$performance == min(tempData$performance)])
    # Get prob distribution
    minVal <- min(data[data$instance == func & data$budget == budg, c("performance")])
    maxVal <- max(data[data$instance == func & data$budget == budg, c("performance")])
    data[data$instance == func & data$budget == budg, c("relativePerformance")] <-  (maxVal - data[data$instance == func & data$budget == budg, c("performance")])/(maxVal - minVal)
    sumVal <- sum(data[data$instance == func & data$budget == budg, c("relativePerformance")])
    data[data$instance == func & data$budget == budg, c("relativePerformance")] <-  data[data$instance == func & data$budget == budg, c("relativePerformance")]/sumVal
    tempVals <- data[data$instance == func & data$budget == budg, c("initialBudgetHigh")]
    tempProbs <- data[data$instance == func & data$budget == budg, c("relativePerformance")]
    meanVal <- experimentalExpectedValue(tempVals, tempProbs)
    meanBest <- c(meanBest, meanVal)
    deviationBest <- c(deviationBest, sqrt(experimentalVariance(tempVals, tempProbs, meanVal)))
  }
}

bestPerformance <- data.frame("instance" = instance,
                              "budget" = budget,
                              "bestInitialBudget" = bestInitialBudget,
                              "meanBestInitialBudget" = meanBest,
                              "deviationBestInitialBudget" = deviationBest)



# Get distribution of top % performing choices
for(perc in percentages){
  meanBest <- c()
  deviationBest <- c()
  for(func in functions){
    for(budg in budgets){
      # Extract data
      tempData <- data[data$instance == func & data$budget == budg, ]
      # Get number of top
      top <- perc * nrow(tempData)
      # Order data based on performance
      tempData <- tempData[order(-tempData$performance), ]
      vals <- tempData[1:top, "initialBudgetHigh"]
      meanBest <- c(meanBest, mean(vals))
      deviationBest <- c(deviationBest, sqrt(var(vals)))
    }
  }
  bestPerformance[, paste0("meanTop", perc)] <- meanBest
  bestPerformance[, paste0("standardDevTop", perc)] <- deviationBest
}


comparisonPerformance <- data.frame("instance" = instance,
                                    "budget" = budget)

for(inst in instance){
  for(budg in budget){
    comparisonPerformance[comparisonPerformance$instance == inst & 
                            comparisonPerformance$budget == budg,
                          c("bestTechnique")] = min(averageData[averageData$instance == inst &
                                                                  averageData$budget == budg,
                                                                c("performance")])
    
    comparisonPerformance[comparisonPerformance$instance == inst & 
                            comparisonPerformance$budget == budg,
                            c("allPointsDown")] = averageData[averageData$instance == inst &
                                                            averageData$budget == budg &
                                                            averageData$initialBudgetHigh == 1.0,
                                                            c("performance")]
    
    comparisonPerformance[comparisonPerformance$instance == inst & 
                            comparisonPerformance$budget == budg,
                          c("allPointsDownComparitivePerformance")] = comparisonPerformance[comparisonPerformance$instance == inst & 
                                                                                              comparisonPerformance$budget == budg,
                                                                                            c("allPointsDown")] / comparisonPerformance[comparisonPerformance$instance == inst & 
                                                                                              comparisonPerformance$budget == budg,
                                                                                            c("bestTechnique")]
  }
}




# Now to plot the info!
png(paste0("../data/sequentialAnalysis/", runName, "-comparisonPerformance.png"), width = 682, height = 682)
plot(c(), c(),
     col = "black",
     type = "p",
     ylab = "Ratio to best strategy",
     xlab = "Total budget",
     #ylim = c(0.9 * min(comparisonPerformance$allPointsDownComparitivePerformance), 1.1 * max(comparisonPerformance$allPointsDownComparitivePerformance)),
     ylim = c(0.9, 3.5),
     xlim = c(5, 35),
     # xaxt = "n",
     pch = 20)

pchs <- c()
num <- 0

for(func in functions){
  lines(comparisonPerformance$budget[comparisonPerformance$instance == func], comparisonPerformance$allPointsDownComparitivePerformance[comparisonPerformance$instance == func], type = "b", pch = num, col = "black")
  pchs <- c(pchs, num)
  num <- num + 1
}
legend("topleft", legend = functions, pch = pchs, col = "black", ncol = 3)
out <- dev.off()







# Now to plot the info!
png(paste0("../data/sequentialAnalysis/", runName, ".png"), width = 682, height = 682)
plot(c(), c(),
     col = "black",
     type = "p",
     ylab = "Best initial budget portion",
     xlab = "Total budget",
     ylim = c(-0.1, 1.5),
     xlim = c(5, 35),
     # xaxt = "n",
     pch = 20)

pchs <- c()
num <- 0

for(func in functions){
  lines(bestPerformance$budget[bestPerformance$instance == func], bestPerformance$bestInitialBudget[bestPerformance$instance == func], type = "b", pch = num, col = "black")
  pchs <- c(pchs, num)
  num <- num + 1
}
smallSet <- c()
bigSet <- c()
xAxis <- c()
for(i in seq(10, 30, 5)){
  smallSet <- c(smallSet, 3.0 / i)
  bigSet <- c(bigSet, 6 / i)
  xAxis <- c(xAxis, i)
}
lines(xAxis, smallSet, pch = num + 1, col = "red")
lines(xAxis, bigSet, pch = num + 2, col = "red")

legend("topleft", legend = functions, pch = pchs, col = "black", ncol = 3)
out <- dev.off()





# Now to plot the info!
png(paste0("../data/sequentialAnalysis/", runName, "-meanAnalysis.png"), width = 682, height = 682)
plot(c(), c(),
     col = "black",
     type = "p",
     ylab = "Best initial budget portion",
     xlab = "Total budget",
     ylim = c(-0.1, 1.5),
     xlim = c(5, 35),
     # xaxt = "n",
     pch = 20)

pchs <- c()
num <- 0

for(func in functions){
  lines(bestPerformance$budget[bestPerformance$instance == func], bestPerformance$meanBestInitialBudget[bestPerformance$instance == func], type = "b", pch = num, col = "black")
  pchs <- c(pchs, num)
  num <- num + 1
}
smallSet <- c()
bigSet <- c()
xAxis <- c()
for(i in seq(10, 30, 5)){
  smallSet <- c(smallSet, 3.0 / i)
  bigSet <- c(bigSet, 6 / i)
  xAxis <- c(xAxis, i)
}
lines(xAxis, smallSet, pch = num + 1, col = "red")
lines(xAxis, bigSet, pch = num + 2, col = "red")

legend("topleft", legend = functions, pch = pchs, col = "black", ncol = 3)
out <- dev.off()





# Now to plot the info!
for(perc in percentages){
  png(paste0("../data/sequentialAnalysis/", runName, "-meanAnalysis-top", perc, ".png"), width = 682, height = 682)
  plot(c(), c(),
       col = "black",
       type = "p",
       ylab = "Best initial budget portion",
       xlab = "Total budget",
       ylim = c(-0.1, 1.5),
       xlim = c(5, 35),
       # xaxt = "n",
       pch = 20)
  
  pchs <- c()
  num <- 0
  
  for(func in functions){
    lines(bestPerformance$budget[bestPerformance$instance == func], bestPerformance[bestPerformance$instance == func, paste0("meanTop", perc)], type = "b", pch = num, col = "black")
    pchs <- c(pchs, num)
    num <- num + 1
  }
  smallSet <- c()
  bigSet <- c()
  xAxis <- c()
  for(i in seq(10, 30, 5)){
    smallSet <- c(smallSet, 3.0 / i)
    bigSet <- c(bigSet, 6 / i)
    xAxis <- c(xAxis, i)
  }
  lines(xAxis, smallSet, pch = num + 1, col = "red")
  lines(xAxis, bigSet, pch = num + 2, col = "red")
  
  legend("topleft", legend = functions, pch = pchs, col = "black", ncol = 3)
  out <- dev.off()
}






