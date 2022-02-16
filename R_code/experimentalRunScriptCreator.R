source("R_code/libraries.R")
# This function eases the creation of the experimental specifications for 
# experiments with the same single source, high fi, and low fi budgets, and
# the same seeds per run.
createScriptStructure <- function(functions, singleSourceBudget, highFiBudget, lowFiBudget, seeds, seedsPerRun, doKrig, doCoKrig, printInfo = FALSE){
  jobs <- 0
  runData <- data.frame(matrix(ncol = 5, nrow = 0))
  if(doKrig){
    for(func in functions){
      for(seedStart in seq(1, seeds, seedsPerRun)){
        jobs <- jobs + 1
        if(printInfo){print(paste0("Working on job ", jobs, " for krig"))}
        data <- c(func, "kriging", singleSourceBudget, 0, paste0(seedStart, "-", min(seedStart + seedsPerRun - 1, seeds)))
        runData[jobs, ] <- data
      }
    }
  }
  if(doCoKrig){
    for(func in functions){
      for(seedStart in seq(1, seeds, seedsPerRun)){
        jobs <- jobs + 1
        if(printInfo){print(paste0("Working on job ", jobs, " for cokrig"))}
        data <- c(func, "cokriging", highFiBudget, lowFiBudget, paste0(seedStart, "-", min(seedStart + seedsPerRun - 1, seeds)))
        runData[jobs, ] <- data
      }
    }
  }
  colnames(runData) <- c("instance", "technique", "highFiBudget", "lowFiBudget", "seeds")
  if(printInfo){print(paste0("Created script with ", jobs, " jobs"))}
  return(runData)
}



