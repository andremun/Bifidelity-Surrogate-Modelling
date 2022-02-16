source("R_code/libraries.R")
source("R_code/experimentalRunScriptCreator.R")
options(warn = -1)

# Script which creates the experimental run for the literature test suite.
# Seeds per run decreases for larger dimensions as it takes longer to run,
# this is done so that each experimental specification takes a similar amount of
# time to run.
print("CREATING LITERATURE SUITE EXPERIMENTAL SCRIPT.")
for(dim in 1:50){
  if(!file.exists(paste0("data/availableFunctions/biSourceDim", dim, ".txt"))){next}
  functions <- read.table(paste0("data/availableFunctions/biSourceDim", dim, ".txt"), header = FALSE, sep = " ", fill = TRUE)[[1]]
  if(dim <= 4){seedsPerRun <- 20}
  else if(dim <= 9){seedsPerRun <- 10}
  else if(dim <= 15){seedsPerRun <- 5}
  else{seedsPerRun <- 2}
  
  runData <- createScriptStructure(functions, 5*dim, 4*dim, dim*10, 20, seedsPerRun, TRUE, TRUE)
  if(dim != 1){
    existingRunData <- read.table("data/runScripts/litSuiteTesting.txt", header = TRUE, sep = " ", fill = TRUE)
    runData <- rbind(existingRunData, runData)
  }
  write.table(runData, "data/runScripts/litSuiteTesting.txt", quote = FALSE, row.names = FALSE)
}

print("CREATED LITERATURE SUITE EXPERIMENTAL SCRIPT.")

