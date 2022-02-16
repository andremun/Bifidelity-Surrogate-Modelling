source("R_code/libraries.R")
options(warn = -1)
# This script generates an experimental run for 81000 instances,
# where the run only calculates the features of each of the new instances.
# This is done so that a subset can then be chosen.
print("CREATING NEW INSTANCES FEATURE ANALYSIS SCRIPT.")
functions <- 1:20
dims <- c(1, 2, 5, 10)
centres <- c(1, 3, 6, 9)
count <- 0
radii <- c(0.025, seq(0.05, 0.25, 0.05))
heights <- c(0, 0.25, 0.5, 0.75, 1)
amps <- c(0.1, 0.5, 1.0, 1.5, 2.0)
freqs <- c(10, 100)
instances <- c()
# First create height based disturbances
for(func in 1:20){
  for(dim in dims){
    if(dim == 1 & func%in%c(8, 9, 17, 18, 19)){next}
    for(height in heights){
      for(radius in radii){
        for(amp in amps){
          for(freq in freqs){
            instances <- c(instances, paste0("COCOfunction", func, "-dim", dim, "-seed1-disth1-height", height, "-radius", radius, "-freq", freq, "-amp", amp))
            instances <- c(instances, paste0("COCOfunction", func, "-dim", dim, "-seed1-disth2-height", height, "-radius", radius, "-freq", freq, "-amp", amp))
            cat(paste0("\rCreated ", length(instances), " instances "))
          }
        }
      }
    }
  }
}
# Now source based disturbances
for(func in 1:20){
  for(dim in dims){
    if(dim == 1 & func%in%c(8, 9, 17, 18, 19)){next}
    for(centre in centres){
      for(radius in radii){
        for(amp in amps){
          for(freq in freqs){
            instances <- c(instances, paste0("COCOfunction", func, "-dim", dim, "-seed1-dists1-centres", centre, "-radius", radius, "-freq", freq, "-amp", amp))
            instances <- c(instances, paste0("COCOfunction", func, "-dim", dim, "-seed1-dists2-centres", centre, "-radius", radius, "-freq", freq, "-amp", amp))
            cat(paste0("\rCreated ", length(instances), " instances "))
          }
        }
      }
    }
  }
}
cat(" - done.\n")
# Create run, just give sample budget of 0 so that only feature is calculated
runData <- data.frame("instance" = instances)
runData$technique <- "kriging"
runData$highFiBudget <- 0
runData$lowFiBudget <- 0
runData$seeds <- "1-1"
write.table(runData, paste0("data/runScripts/newInstancesFeatures.txt"), quote = FALSE, row.names = FALSE)

print("CREATED NEW INSTANCES FEATURE ANALYSIS SCRIPT.")

