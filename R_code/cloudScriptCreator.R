library("stringr")   

# Scripts used to create files which specify experimental runs.
# These scripts can be modified to create user specific experimental runs.
# The experimental runs specified below are used in the study 
# "Bi-fidelity Surrogate Modelling: Showcasing the need for new test instances" (2022)
# by Andres-Thio N, Munoz MA and Smith-Miles K.



# This function eases the creation of the experimental specifications for 
# experiments with the same single source, high fi, and low fi budgets, and
# the same seeds per run.
createScriptStructure <- function(functions, singleSourceBudget, highFiBudget, lowFiBudget, seeds, seedsPerRun, doKrig, doCoKrig){
  jobs <- 0
  runData <- data.frame(matrix(ncol = 5, nrow = 0))
  if(doKrig){
    for(func in functions){
      for(seedStart in seq(1, seeds, seedsPerRun)){
        jobs <- jobs + 1
        print(paste0("Working on job ", jobs, " for krig"))
        data <- c(func, "kriging", singleSourceBudget, 0, paste0(seedStart, "-", min(seedStart + seedsPerRun - 1, seeds)))
        runData[jobs, ] <- data
      }
    }
  }
  if(doCoKrig){
    for(func in functions){
      for(seedStart in seq(1, seeds, seedsPerRun)){
        jobs <- jobs + 1
        print(paste0("Working on job ", jobs, " for cokrig"))
        data <- c(func, "cokriging", highFiBudget, lowFiBudget, paste0(seedStart, "-", min(seedStart + seedsPerRun - 1, seeds)))
        runData[jobs, ] <- data
      }
    }
  }
  colnames(runData) <- c("instance", "technique", "highFiBudget", "lowFiBudget", "seeds")
  print(paste0("Created script with ", jobs, " jobs"))
  return(runData)
}



# Script which creates the experimental run for the literature test suite.
# Seeds per run decreases for larger dimensions as it takes longer to run,
# this is done so that each experimental specification takes a similar amount of
# time to run.
for(dim in 1:50){
  if(!file.exists(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"))){next}
  functions <- read.table(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"), header = FALSE, sep = " ", fill = TRUE)[[1]]
  if(dim <= 4){seedsPerRun <- 20}
  else if(dim <= 9){seedsPerRun <- 10}
  else if(dim <= 15){seedsPerRun <- 5}
  else{seedsPerRun <- 2}
  
  runDataAccuracy <- createScriptStructure(functions, 5*dim, dim, 4*dim, 20, seedsPerRun, TRUE, TRUE)
  if(dim != 1){
    existingRunDataAccuracy <- read.table("../data/runScripts/litSuiteTesting.txt", header = TRUE, sep = " ", fill = TRUE)
    runDataAccuracy <- rbind(existingRunDataAccuracy, runDataAccuracy)
  }
  write.table(runDataAccuracy, "../data/runScripts/litSuiteTesting.txt", quote = FALSE, row.names = FALSE)
  print(paste0("Dim ", dim))
}




# Script for interesting COCO functions
# mult <- 10
# runDataD1 <- createScriptStructure(read.table("../data/availableFunctions/COCOInterestingFunctionsd1.txt", header = FALSE, sep = " ", fill = TRUE)[[1]], 
#                                    c(1*mult), c(1.0), c(0.8), c(0.2), c(0.1), 20, 20, TRUE, TRUE)
# 
# runDataD2 <- createScriptStructure(read.table("../data/availableFunctions/COCOInterestingFunctionsd2.txt", header = FALSE, sep = " ", fill = TRUE)[[1]], 
#                                    c(2*mult), c(1.0), c(0.8), c(0.2), c(0.1), 20, 20, TRUE, TRUE)
# 
# runDataD5 <- createScriptStructure(read.table("../data/availableFunctions/COCOInterestingFunctionsd5.txt", header = FALSE, sep = " ", fill = TRUE)[[1]], 
#                                    c(5*mult), c(1.0), c(0.8), c(0.2), c(0.1), 20, 20, TRUE, TRUE)
# 
# runData <- rbind(rbind(runDataD1, runDataD2), runDataD5)
# 
# write.table(runData, paste0("../data/runScripts/COCOinterestingFunctionsBudget", mult, ".txt"), quote = FALSE, row.names = FALSE)


functions <- c("COCOfunction12-dim10-seed1-globalNoise10-centres3-radius0.2-noise2-freq10-amp1",
               "COCOfunction12-dim10-seed1-globalNoise10-centres3-radius0.2-noise2-freq100-amp1.5",
               "COCOfunction12-dim10-seed1-globalNoise10-centres9-radius0.2-noise2-freq100-amp2",
               "COCOfunction12-dim10-seed1-globalNoise10-centres3-radius0.25-noise2-freq10-amp1.5",
               "COCOfunction12-dim10-seed1-globalNoise10-centres6-radius0.2-noise2-freq10-amp1.5")

runDataAccuracy <- rbind(createScriptStructure("COCOfunction12-dim5-seed1-globalNoise5-height0.25-radius0.2-noise2-freq100-amp1.5", 25, 1.0, 0.8, 0.2, 0.1, 20, 1, FALSE, TRUE, "modelAccuracy"),
                         createScriptStructure(functions, 50, 1.0, 0.8, 0.2, 0.1, 20, 1, FALSE, TRUE, "modelAccuracy"))

write.table(runDataAccuracy, "../data/runScripts/miniTest.txt", quote = FALSE, row.names = FALSE)








highFiBudgets <- c(0.8, 0.6, 0.4, 0.2)
lowFiBudgets <- c(0.2, 0.4, 0.6, 0.8)
# Expanded runs to feed to instance space
# Start with model accuracy
# First lit test suite
for(dim in 1:50){
  if(!file.exists(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"))){next}
  functions <- read.table(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"), header = FALSE, sep = " ", fill = TRUE)[[1]]
  if(dim <= 4){seedsPerRun <- 20}
  else if(dim <= 9){seedsPerRun <- 10}
  else if(dim <= 15){seedsPerRun <- 5}
  else{seedsPerRun <- 2}
  runDataAccuracy <- createScriptStructure(functions, c(5*dim), c(1.0), highFiBudgets, lowFiBudgets, (0.1), 20, seedsPerRun, TRUE, TRUE, "modelAccuracy")
  if(dim != 1){
    existingRunDataAccuracy <- read.table("../data/runScripts/litSuiteSurrogateModelFitExpandedCoKrig.txt", header = TRUE, sep = " ", fill = TRUE)
    runDataAccuracy <- rbind(existingRunDataAccuracy, runDataAccuracy)
  }
  write.table(runDataAccuracy, "../data/runScripts/litSuiteSurrogateModelFitExpandedCoKrig.txt", quote = FALSE, row.names = FALSE)
  print(paste0("Dim ", dim))
}

# Now with the two new test suites
# Have a new test suite, do this one instead
allFunctions <- read.table("../data/availableFunctions/COCOsubsetDataLocalCorr0-5.txt", header = FALSE, sep = " ", fill = TRUE)[[1]]
for(dim in c(1, 2, 5, 10)){
  if(dim <= 4){seedsPerRun <- 20}
  else if(dim <= 9){seedsPerRun <- 10}
  else if(dim <= 15){seedsPerRun <- 4}
  else{seedsPerRun <- 2}
  functions <- allFunctions[str_which(allFunctions, paste0("-dim", dim, "-"), negate = FALSE)]
  runDataAccuracy <- createScriptStructure(functions, c(5*dim), c(1.0), highFiBudgets, lowFiBudgets, (0.1), 20, seedsPerRun, TRUE, TRUE, "modelAccuracy")
  if(dim != 1){
    existingRunDataAccuracy <- read.table("../data/runScripts/finalModelAccuracySuiteSurrogateModelFitExpandedCoKrig.txt", header = TRUE, sep = " ", fill = TRUE)
    runDataAccuracy <- rbind(existingRunDataAccuracy, runDataAccuracy)
  }
  write.table(runDataAccuracy, "../data/runScripts/finalModelAccuracySuiteSurrogateModelFitExpandedCoKrig.txt", quote = FALSE, row.names = FALSE)
  print(paste0("Dim ", dim))
}










# USED THIS FOR THE PAPER
# # Start with model accuracy
# # First lit test suite
# for(dim in 1:50){
#   if(!file.exists(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"))){next}
#   functions <- read.table(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"), header = FALSE, sep = " ", fill = TRUE)[[1]]
#   if(dim <= 4){seedsPerRun <- 20}
#   else if(dim <= 9){seedsPerRun <- 10}
#   else if(dim <= 15){seedsPerRun <- 5}
#   else{seedsPerRun <- 2}
#   runDataAccuracy <- createScriptStructure(functions, c(5*dim), c(1.0), c(0.8), (0.2), (0.1), 20, seedsPerRun, TRUE, TRUE, "modelAccuracy")
#   if(dim != 1){
#     existingRunDataAccuracy <- read.table("../data/runScripts/01-litSuiteSurrogateModelFit.txt", header = TRUE, sep = " ", fill = TRUE)
#     runDataAccuracy <- rbind(existingRunDataAccuracy, runDataAccuracy)
#   }
#   write.table(runDataAccuracy, "../data/runScripts/01-litSuiteSurrogateModelFit.txt", quote = FALSE, row.names = FALSE)
#   print(paste0("Dim ", dim))
# }
# 
# # Now with the two new test suites
# # Have a new test suite, do this one instead
# allFunctions <- read.table("../data/availableFunctions/COCOsubsetDataLocalCorr0-5.txt", header = FALSE, sep = " ", fill = TRUE)[[1]]
# for(dim in c(1, 2, 5, 10)){
#   if(dim <= 4){seedsPerRun <- 20}
#   else if(dim <= 9){seedsPerRun <- 10}
#   else if(dim <= 15){seedsPerRun <- 4}
#   else{seedsPerRun <- 2}
#   functions <- allFunctions[str_which(allFunctions, paste0("-dim", dim, "-"), negate = FALSE)]
#   runDataAccuracy <- createScriptStructure(functions, c(5*dim), c(1.0), c(0.8), (0.2), (0.1), 20, seedsPerRun, TRUE, TRUE, "modelAccuracy")
#   if(dim != 1){
#     existingRunDataAccuracy <- read.table("../data/runScripts/00-finalModelAccuracySuiteSurrogateModelFit.txt", header = TRUE, sep = " ", fill = TRUE)
#     runDataAccuracy <- rbind(existingRunDataAccuracy, runDataAccuracy)
#   }
#   write.table(runDataAccuracy, "../data/runScripts/00-finalModelAccuracySuiteSurrogateModelFit.txt", quote = FALSE, row.names = FALSE)
#   print(paste0("Dim ", dim))
# }





# # Start with model accuracy suite
# allFunctions <- read.table("../data/availableFunctions/COCOsubsetDataModelAccuracyR2.txt", header = FALSE, sep = " ", fill = TRUE)[[1]]
# for(dim in c(1, 2, 5, 10)){
#   if(dim <= 4){seedsPerRun <- 20}
#   else if(dim <= 9){seedsPerRun <- 10}
#   else if(dim <= 15){seedsPerRun <- 4}
#   else{seedsPerRun <- 2}
#   functions <- allFunctions[str_which(allFunctions, paste0("-dim", dim, "-"), negate = FALSE)]
#   runDataAccuracy <- createScriptStructure(functions, c(5*dim), c(1.0), c(0.8), (0.2), (0.1), 20, seedsPerRun, TRUE, TRUE, "modelAccuracy")
#   if(dim != 1){
#     existingRunDataAccuracy <- read.table("../data/runScripts/04-modelAccuracySuiteSurrogateModelFit.txt", header = TRUE, sep = " ", fill = TRUE)
#     runDataAccuracy <- rbind(existingRunDataAccuracy, runDataAccuracy)
#   }
#   write.table(runDataAccuracy, "../data/runScripts/04-modelAccuracySuiteSurrogateModelFit.txt", quote = FALSE, row.names = FALSE)
#   print(paste0("Dim ", dim))
# }
# 
# # Now function optimisation suite
# allFunctions <- read.table("../data/availableFunctions/COCOsubsetDataFunctionOptimisationR2.txt", header = FALSE, sep = " ", fill = TRUE)[[1]]
# for(dim in c(1, 2, 5, 10)){
#   if(dim <= 4){seedsPerRun <- 20}
#   else if(dim <= 9){seedsPerRun <- 10}
#   else if(dim <= 15){seedsPerRun <- 4}
#   else{seedsPerRun <- 2}
#   functions <- allFunctions[str_which(allFunctions, paste0("-dim", dim, "-"), negate = FALSE)]
#   runDataAccuracy <- createScriptStructure(functions, c(5*dim), c(1.0), c(0.8), (0.2), (0.1), 20, seedsPerRun, TRUE, TRUE, "modelAccuracy")
#   if(dim != 1){
#     existingRunDataAccuracy <- read.table("../data/runScripts/07-functionOptSuiteSurrogateModelFit.txt", header = TRUE, sep = " ", fill = TRUE)
#     runDataAccuracy <- rbind(existingRunDataAccuracy, runDataAccuracy)
# 
#   }
#   write.table(runDataAccuracy, "../data/runScripts/07-functionOptSuiteSurrogateModelFit.txt", quote = FALSE, row.names = FALSE)
#   print(paste0("Dim ", dim))
# }
# 
# 
# 
# # Ok now can work for surrogate model exploration and surrogate model exploitation
# # Start with lit instances, separate into dim < 9 and the rest
# for(dim in 1:9){
#   if(!file.exists(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"))){next}
#   functions <- read.table(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"), header = FALSE, sep = " ", fill = TRUE)[[1]]
#   if(dim <= 3){seedsPerRun <- 20}
#   else if(dim == 4){seedsPerRun <- 10}
#   else if(dim == 5){seedsPerRun <- 5}
#   else if(dim == 6){seedsPerRun <- 3}
#   else{seedsPerRun <- 1}
#   runDataExploration <- createScriptStructure(functions, c(10*dim), c(0.5), c(0.4), (0.1), (0.1), 20, seedsPerRun, TRUE, TRUE, "modelAccuracy")
#   runDataOptimisation <- createScriptStructure(functions, c(10*dim), c(0.5), c(0.4), (0.1), (0.1), 20, seedsPerRun, TRUE, TRUE, "functionOpt")
#   if(dim != 1){
#     existingRunDataExploration <- read.table("../data/runScripts/02a-litSuiteSurrogateModelExploration.txt", header = TRUE, sep = " ", fill = TRUE)
#     runDataExploration <- rbind(existingRunDataExploration, runDataExploration)
#     
#     existingRunDataOptimisation <- read.table("../data/runScripts/03a-litSuiteSurrogateModelExploitation.txt", header = TRUE, sep = " ", fill = TRUE)
#     runDataOptimisation <- rbind(existingRunDataOptimisation, runDataOptimisation)
#   }
#   write.table(runDataExploration, "../data/runScripts/02a-litSuiteSurrogateModelExploration.txt", quote = FALSE, row.names = FALSE)
#   write.table(runDataOptimisation, "../data/runScripts/03a-litSuiteSurrogateModelExploitation.txt", quote = FALSE, row.names = FALSE)
#   print(paste0("Dim ", dim))
# }
# 
# 
# for(dim in 10:50){
#   if(!file.exists(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"))){next}
#   functions <- read.table(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"), header = FALSE, sep = " ", fill = TRUE)[[1]]
#   if(dim <= 3){seedsPerRun <- 20}
#   else if(dim == 4){seedsPerRun <- 10}
#   else if(dim == 5){seedsPerRun <- 5}
#   else if(dim == 6){seedsPerRun <- 3}
#   else{seedsPerRun <- 1}
#   runDataExploration <- createScriptStructure(functions, c(10*dim), c(0.5), c(0.4), (0.1), (0.1), 20, seedsPerRun, TRUE, TRUE, "modelAccuracy")
#   runDataOptimisation <- createScriptStructure(functions, c(10*dim), c(0.5), c(0.4), (0.1), (0.1), 20, seedsPerRun, TRUE, TRUE, "functionOpt")
#   if(dim != 10){
#     existingRunDataExploration <- read.table("../data/runScripts/02b-litSuiteSurrogateModelExploration.txt", header = TRUE, sep = " ", fill = TRUE)
#     runDataExploration <- rbind(existingRunDataExploration, runDataExploration)
#     
#     existingRunDataOptimisation <- read.table("../data/runScripts/03b-litSuiteSurrogateModelExploitation.txt", header = TRUE, sep = " ", fill = TRUE)
#     runDataOptimisation <- rbind(existingRunDataOptimisation, runDataOptimisation)
#   }
#   write.table(runDataExploration, "../data/runScripts/02b-litSuiteSurrogateModelExploration.txt", quote = FALSE, row.names = FALSE)
#   write.table(runDataOptimisation, "../data/runScripts/03b-litSuiteSurrogateModelExploitation.txt", quote = FALSE, row.names = FALSE)
#   print(paste0("Dim ", dim))
# }
# 
# 
# # Finish with new test suites, but separate them as well
# allFunctions <- read.table("../data/availableFunctions/COCOsubsetDataModelAccuracyR2.txt", header = FALSE, sep = " ", fill = TRUE)[[1]]
# for(dim in c(1, 2, 5)){
#   if(dim <= 3){seedsPerRun <- 20}
#   else if(dim == 4){seedsPerRun <- 10}
#   else if(dim == 5){seedsPerRun <- 5}
#   else if(dim == 6){seedsPerRun <- 3}
#   else{seedsPerRun <- 1}
#   functions <- allFunctions[str_which(allFunctions, paste0("-dim", dim, "-"), negate = FALSE)]
#   runDataExploration <- createScriptStructure(functions, c(10*dim), c(0.5), c(0.4), (0.1), (0.1), 20, seedsPerRun, TRUE, TRUE, "modelAccuracy")
#   runDataOptimisation <- createScriptStructure(functions, c(10*dim), c(0.5), c(0.4), (0.1), (0.1), 20, seedsPerRun, TRUE, TRUE, "functionOpt")
#   if(dim != 1){
#     existingRunDataExploration <- read.table("../data/runScripts/05a-modelAccuracySuiteSurrogateModelExploration.txt", header = TRUE, sep = " ", fill = TRUE)
#     runDataExploration <- rbind(existingRunDataExploration, runDataExploration)
#     
#     existingRunDataOptimisation <- read.table("../data/runScripts/06a-modelAccuracySuiteSurrogateModelExploitation.txt", header = TRUE, sep = " ", fill = TRUE)
#     runDataOptimisation <- rbind(existingRunDataOptimisation, runDataOptimisation)
#   }
#   write.table(runDataExploration, "../data/runScripts/05a-modelAccuracySuiteSurrogateModelExploration.txt", quote = FALSE, row.names = FALSE)
#   write.table(runDataOptimisation, "../data/runScripts/06a-modelAccuracySuiteSurrogateModelExploitation.txt", quote = FALSE, row.names = FALSE)
#   print(paste0("Dim ", dim))
# }
# 
# for(dim in c(10)){
#   if(dim <= 3){seedsPerRun <- 20}
#   else if(dim == 4){seedsPerRun <- 10}
#   else if(dim == 5){seedsPerRun <- 5}
#   else if(dim == 6){seedsPerRun <- 3}
#   else{seedsPerRun <- 1}
#   functions <- allFunctions[str_which(allFunctions, paste0("-dim", dim, "-"), negate = FALSE)]
#   runDataExploration <- createScriptStructure(functions, c(10*dim), c(0.5), c(0.4), (0.1), (0.1), 20, seedsPerRun, TRUE, TRUE, "modelAccuracy")
#   runDataOptimisation <- createScriptStructure(functions, c(10*dim), c(0.5), c(0.4), (0.1), (0.1), 20, seedsPerRun, TRUE, TRUE, "functionOpt")
#   write.table(runDataExploration, "../data/runScripts/05b-modelAccuracySuiteSurrogateModelExploration.txt", quote = FALSE, row.names = FALSE)
#   write.table(runDataOptimisation, "../data/runScripts/06b-modelAccuracySuiteSurrogateModelExploitation.txt", quote = FALSE, row.names = FALSE)
#   print(paste0("Dim ", dim))
# }
# 
# 
# 
# allFunctions <- read.table("../data/availableFunctions/COCOsubsetDataFunctionOptimisationR2.txt", header = FALSE, sep = " ", fill = TRUE)[[1]]
# for(dim in c(1, 2, 5)){
#   if(dim <= 3){seedsPerRun <- 20}
#   else if(dim == 4){seedsPerRun <- 10}
#   else if(dim == 5){seedsPerRun <- 5}
#   else if(dim == 6){seedsPerRun <- 3}
#   else{seedsPerRun <- 1}
#   functions <- allFunctions[str_which(allFunctions, paste0("-dim", dim, "-"), negate = FALSE)]
#   runDataExploration <- createScriptStructure(functions, c(10*dim), c(0.5), c(0.4), (0.1), (0.1), 20, seedsPerRun, TRUE, TRUE, "modelAccuracy")
#   runDataOptimisation <- createScriptStructure(functions, c(10*dim), c(0.5), c(0.4), (0.1), (0.1), 20, seedsPerRun, TRUE, TRUE, "functionOpt")
#   if(dim != 1){
#     existingRunDataExploration <- read.table("../data/runScripts/08a-functionOptSuiteSurrogateModelExploration.txt", header = TRUE, sep = " ", fill = TRUE)
#     runDataExploration <- rbind(existingRunDataExploration, runDataExploration)
#     
#     existingRunDataOptimisation <- read.table("../data/runScripts/09a-functionOptSuiteSurrogateModelExploitation.txt", header = TRUE, sep = " ", fill = TRUE)
#     runDataOptimisation <- rbind(existingRunDataOptimisation, runDataOptimisation)
#   }
#   write.table(runDataExploration, "../data/runScripts/08a-functionOptSuiteSurrogateModelExploration.txt", quote = FALSE, row.names = FALSE)
#   write.table(runDataOptimisation, "../data/runScripts/09a-functionOptSuiteSurrogateModelExploitation.txt", quote = FALSE, row.names = FALSE)
#   print(paste0("Dim ", dim))
# }
# 
# for(dim in c(10)){
#   if(dim <= 3){seedsPerRun <- 20}
#   else if(dim == 4){seedsPerRun <- 10}
#   else if(dim == 5){seedsPerRun <- 5}
#   else if(dim == 6){seedsPerRun <- 3}
#   else{seedsPerRun <- 1}
#   functions <- allFunctions[str_which(allFunctions, paste0("-dim", dim, "-"), negate = FALSE)]
#   runDataExploration <- createScriptStructure(functions, c(10*dim), c(0.5), c(0.4), (0.1), (0.1), 20, seedsPerRun, TRUE, TRUE, "modelAccuracy")
#   runDataOptimisation <- createScriptStructure(functions, c(10*dim), c(0.5), c(0.4), (0.1), (0.1), 20, seedsPerRun, TRUE, TRUE, "functionOpt")
#   write.table(runDataExploration, "../data/runScripts/08b-functionOptSuiteSurrogateModelExploration.txt", quote = FALSE, row.names = FALSE)
#   write.table(runDataOptimisation, "../data/runScripts/09b-functionOptSuiteSurrogateModelExploitation.txt", quote = FALSE, row.names = FALSE)
#   print(paste0("Dim ", dim))
# }









# Have a script with 0 budget just to get features of the functions I am interested in
allFunctions <- as.character(read.table("../data/availableFunctions/biSourceAll.txt", header = FALSE, sep = " ", fill = TRUE)[[1]])
#allFunctions <- c(allFunctions, as.character(read.table("../data/availableFunctions/COCOsubsetDataModelAccuracyR2.txt", header = FALSE, sep = " ", fill = TRUE)[[1]]))
#allFunctions <- c(allFunctions, as.character(read.table("../data/availableFunctions/COCOsubsetDataFunctionOptimisationR2.txt", header = FALSE, sep = " ", fill = TRUE)[[1]]))
allFunctions <- c(allFunctions, as.character(read.table("../data/availableFunctions/COCOsubsetDataLocalCorr0-5.txt", header = FALSE, sep = " ", fill = TRUE)[[1]]))
allFunctions <- unique(as.character(allFunctions))
runData <- data.frame("instance" = allFunctions)
runData$sampleStrategy <- "modelAccuracy"
runData$technique <- "kriging"
runData$budget <- 0
runData$initialBudgetHigh <- 0
runData$initialBudgetLow <- 0
runData$costRatio <- 0.1
runData$seeds <- "1-1"
runData <- runData[, c("sampleStrategy", "instance", "technique", "budget", "initialBudgetHigh", "initialBudgetLow", "costRatio", "seeds")]
write.table(runData, paste0("../data/runScripts/allFunctionsFeatures.txt"), quote = FALSE, row.names = FALSE)

# # Want code with modifying budget based on function dimension. Let us try something like this
# budgetMult <- 5
# name <- paste0("modelAccuracyBudget", budgetMult, "d")
# for(dim in 1:50){
#   if(!file.exists(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"))){next}
#   functions <- read.table(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"), header = FALSE, sep = " ", fill = TRUE)[[1]]
#   print(dim)
#   # This section is used for the "standard" model assessment
#   # seeds <- 50
#   # seedsPerRun <- ceiling(seeds * min(1, 2 / dim))
#   # if(dim >= 10){seeds <- seeds / 2}
#   # costRatios <- c(0.1)
#   # budgets <- c(budgetMult * dim)
#   # singleSourceBudgets <- c(1.0)
#   # highFiBudgets <- c(0.8)
#   # lowFiBudgets <- c(0.2)
#   # doKrig <- TRUE
#   # doCoKrig <- TRUE
# 
#   # # This section is used for the "thorough" run with extra
#   # seeds <- 50
#   # seedsPerRun <- ceiling(seeds * min(1, 5 / dim))
#   # if(dim >= 10){seeds <- seeds / 2}
#   # costRatios <- c(0.1)
#   # budgets <- c(budgetMult * dim)
#   # singleSourceBudgets <- c(1.0)
#   # highFiBudgets <- c(0.1, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95)
#   # lowFiBudgets <- c(0.9, 0.8, 0.6, 0.4, 0.2, 0.1, 0.05)
#   # doKrig <- FALSE
#   # doCoKrig <- TRUE
#   # name <- paste0("modelAccuracyBudget", budgetMult, "dExtendedCostRatios")
# 
#   # This section is used for the "thorough" run with many cokriging techniques, budgets and cost ratios
#   # seeds <- 20
#   # seedsPerRun <- ceiling(seeds * min(1, 10 / dim))
#   # #if(dim >= 10){seeds <- seeds / 2}
#   # costRatios <- c(0.25, 0.143, 0.1, 0.067, 0.05, 0.01)
#   # if(dim < 5){budgets <- c(5 * dim, 10 * dim, 15 * dim, 20 * dim)}
#   # else if(dim >= 5 & dim <= 10){budgets <- c(2 * dim, 5 * dim, 10 * dim, 15 * dim)}
#   # else{budgets <- c(1 * dim, 2 * dim, 5 * dim, 10 * dim)}
#   # singleSourceBudgets <- c(1.0)
#   # highFiBudgets <- c(0.2, 0.5, 0.8)
#   # lowFiBudgets <- c(0.8, 0.5, 0.2)
#   # doKrig <- TRUE
#   # doCoKrig <- TRUE
# 
#   name <- paste0("litModelAccuracy")
#   runData <- createScriptStructure(functions, budgets, singleSourceBudgets, highFiBudgets, lowFiBudgets, costRatios, seeds, seedsPerRun, doKrig, doCoKrig)
#   if(dim != 1){
#     existingRunData <- read.table(paste0("../data/runScripts/", name, ".txt"), header = TRUE, sep = " ", fill = TRUE)
#     runData <- rbind(existingRunData, runData)
#   }
#   write.table(runData, paste0("../data/runScripts/", name, ".txt"), quote = FALSE, row.names = FALSE)
#   print(paste0("Dim ", dim, " currently have ", nrow(runData), " jobs"))
# }









# SCRIPT CREATED FOR THE GENERATION OF TEST INSTANCES, TOTAL OF 1008000
# Really just want the functions so need to create them
functions <- c()
dims <- c(1, 2, 5, 10)
centres <- c(1, 3, 6, 9)
count <- 0
radii <- c(0.025, seq(0.05, 0.25, 0.05))
heights <- c(0, 0.25, 0.5, 0.75, 1)
amps <- c(0.1, 0.5, 1.0, 1.5, 2.0)
noises <- c(2)
freqs <- c(10, 100)

for(globalNoise in c(10, 11)){
  for(func in 1:20){
    for(dim in dims){
      if(dim == 1 & func%in%c(8, 9, 17, 18, 19)){next}
      for(centre in centres){
        for(radius in radii){
          for(amp in amps){
            for(noise in noises){
              if(noise == 1 | noise == 2){
                for(freq in freqs){
                  functions <- c(functions, paste0("COCOfunction", func, "-dim", dim, "-seed1-globalNoise", globalNoise, "-centres", centre, "-radius", radius, "-noise", noise, "-freq", freq, "-amp", amp))
                  print(paste0("Done ", length(functions)))
                  #count <- count + 1
                  #print(paste0("Done ", count))
                }
              }else{
                functions <- c(functions, paste0("COCOfunction", func, "-dim", dim, "-seed1-globalNoise", globalNoise, "-centres", centre, "-radius", radius, "-noise", noise, "-amp", amp))
                print(paste0("Done ", length(functions)))
                #count <- count + 1
                #print(paste0("Done ", count))
              }

            }
          }
        }
      }
    }
  }
}
for(globalNoise in c(4, 5)){
  for(func in 1:20){
    for(dim in dims){
      if(dim == 1 & func%in%c(8, 9, 17, 18, 19)){next}
      for(height in heights){
        for(radius in radii){
          for(amp in amps){
            for(noise in noises){
              if(noise == 1 | noise == 2){
                for(freq in freqs){
                  functions <- c(functions, paste0("COCOfunction", func, "-dim", dim, "-seed1-globalNoise", globalNoise, "-height", height, "-radius", radius, "-noise", noise, "-freq", freq, "-amp", amp))
                  print(paste0("Done ", length(functions)))
                  #count <- count + 1
                  #print(paste0("Done ", count))
                }
              }else{
                functions <- c(functions, paste0("COCOfunction", func, "-dim", dim, "-seed1-globalNoise", globalNoise, "-height", height, "-radius", radius, "-noise", noise, "-amp", amp))
                print(paste0("Done ", length(functions)))
                #count <- count + 1
                #print(paste0("Done ", count))
              }
            }
          }
        }
      }
    }
  }
}
runData <- data.frame("instance" = functions)
runData$sampleStrategy <- "modelAccuracy"
runData$technique <- "kriging"
runData$budget <- 0
runData$initialBudgetHigh <- 0
runData$initialBudgetLow <- 0
runData$costRatio <- 0.1
runData$seeds <- "1-1"
runData <- runData[, c("sampleStrategy", "instance", "technique", "budget", "initialBudgetHigh", "initialBudgetLow", "costRatio", "seeds")]
write.table(runData, paste0("../data/runScripts/COCOinstanceFeaturesNew.txt"), quote = FALSE, row.names = FALSE)

# 
# 
# # Do the same for all lit functions
# functions <- c()
# for(dim in 1:50){
#   if(!file.exists(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"))){next}
#   functions <- c(functions, as.character(read.table(paste0("../data/availableFunctions/biSourceDim", dim, ".txt"), header = FALSE, sep = " ", fill = TRUE)[[1]]))
#   print(dim)
#   print(functions)
# }
# runData <- data.frame("instance" = functions)
# runData$sampleStrategy <- "modelAccuracy"
# runData$technique <- "kriging"
# runData$budget <- 0
# runData$initialBudgetHigh <- 0
# runData$initialBudgetLow <- 0
# runData$costRatio <- 0.1
# runData$seeds <- "1-1"
# runData <- runData[, c("sampleStrategy", "instance", "technique", "budget", "initialBudgetHigh", "initialBudgetLow", "costRatio", "seeds")]
# write.table(runData, paste0("../data/runScripts/litInstanceFeatures.txt"), quote = FALSE, row.names = FALSE)




# SCRIPT USED FOR ORIGINAL MODEL PERFORMANCE
# functions <- read.table("../data/availableFunctions/biSourceAll.txt", header = FALSE, sep = " ", fill = TRUE)[[1]]
# #functions <- c("XiongParkFirst", "ShiPark", "ShiCurrinSin", "LiuEllipsoid")
# scriptName <- "modelPerformanceSetup"
# seeds <- 50
# seedsPerRun <- 25
# costRatios <- c(0.1)
# budgets <- c(10)
# singleSourceBudgets <- c(1.0)
# highFiBudgets <- c(0.8)
# lowFiBudgets <- c(0.2)
# doKrig <- TRUE
# doCoKrig <- TRUE
# 
# runData <- createScriptStructure(functions, budgets, singleSourceBudgets, highFiBudgets, lowFiBudgets, costRatios, seeds, seedsPerRun, doKrig, doCoKrig)
# write.table(runData, paste0("../data/runScripts/", scriptName, ".txt"), quote = FALSE, row.names = FALSE)





# # SCRIPT USED FOR TOAL REPRODUCTION, A BIT CLUNKY
# # First work with the Krigs
# krig2d <- createScriptStructure(c("ToalBranin0.00", "ToalPaciorek0.00"), c(10), c(1.0), c(), c(), c(), 50, 50, TRUE, FALSE)
# krig3d <- createScriptStructure(c("ToalHartmannH30.00"), c(15), c(1.0), c(), c(), c(), 50, 50, TRUE, FALSE)
# krig10d <- createScriptStructure(c("ToalTrid0.00"), c(50), c(1.0), c(), c(), c(), 25, 5, TRUE, FALSE)
# # Now the cokrigs
# cokrig2dNames <- c("ToalBranin", "ToalPaciorek")
# cokrig3dNames <- c("ToalHartmannH3")
# cokrig10dNames <- c("ToalTrid")
# vals <- c("0.00", "0.05", "0.10", "0.15", "0.20", "0.25", "0.30", "0.35", "0.40", "0.45", "0.50", "0.55", "0.60", "0.65", "0.70", "0.75", "0.80", "0.85", "0.90", "0.95", "1.00")
# 
# functions <- c()
# for(name in cokrig2dNames){
#   for(val in vals){
#     functions <- c(functions, paste0(name, val))
#   }
# }
# cokrig2d <- createScriptStructure(functions, c(10), c(), c(0.8), c(0.2), c(1.0/4.0, 1.0/7.0, 1.0/10.0, 1.0 / 15.0, 1.0 / 20.0), 50, 50, FALSE, TRUE)
# 
# functions <- c()
# for(name in cokrig3dNames){
#   for(val in vals){
#     functions <- c(functions, paste0(name, val))
#   }
# }
# cokrig3d <- createScriptStructure(functions, c(15), c(), c(0.8), c(0.2), c(1.0/4.0, 1.0/7.0, 1.0/10.0, 1.0 / 15.0, 1.0 / 20.0), 50, 50, FALSE, TRUE)
# 
# functions <- c()
# for(name in cokrig10dNames){
#   for(val in vals){
#     functions <- c(functions, paste0(name, val))
#   }
# }
# cokrig10d <- createScriptStructure(functions, c(50), c(), c(0.8), c(0.2), c(1.0/4.0, 1.0/7.0, 1.0/10.0, 1.0 / 15.0, 1.0 / 20.0), 25, 5, FALSE, TRUE)
# 
# final <- rbind(krig2d, krig3d, krig10d, cokrig2d, cokrig3d, cokrig10d)
# print(paste0("Total of ", nrow(final), " jobs"))
# write.table(final, paste0("../data/runScripts/", "toalReproductionSetup", ".txt"), quote = FALSE, row.names = FALSE)
