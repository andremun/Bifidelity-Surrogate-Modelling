source("R_code/libraries.R")
source("R_code/dataProcessor.R")
source("R_code/plotFeatures.R")
source("R_code/plotPerformance.R")
source("R_code/decisionTreeAnalysis.R")
options(warn = -1)

# This script takes the results stored in the files litSuiteTesting and 
# processes them. That is it takes the results of the literature suite 
# experiments and plots the instance features and the performance of the
# models.
print("ANALYSING LITERATURE SUITE EXPERIMENTS.")
graphScale <- 0.7
literatureTestSuiteResults <- combineAndProcessResults("litSuiteTesting", 1, 906)
# Add columns for information when plotting features
literatureTestSuiteResults$pch = 16
literatureTestSuiteResults$legend = "Fixed"
literatureTestSuiteResults$col = "black"
literatureTestSuiteResults[str_which(literatureTestSuiteResults$instance, "Wang", negate = FALSE), "pch"] = 17
literatureTestSuiteResults[str_which(literatureTestSuiteResults$instance, "Wang", negate = FALSE), "legend"] = "Error-based"
literatureTestSuiteResults[str_which(literatureTestSuiteResults$instance, "Wang", negate = FALSE), "col"] = "sienna3"
literatureTestSuiteResults[str_which(literatureTestSuiteResults$instance, "Toal", negate = FALSE), "pch"] = 15
literatureTestSuiteResults[str_which(literatureTestSuiteResults$instance, "Toal", negate = FALSE), "legend"] = "Parameter-based"
literatureTestSuiteResults[str_which(literatureTestSuiteResults$instance, "Toal", negate = FALSE), "col"] = "sienna4"
# Plot feature graphs
cat("Plotting feature graphs ")
plotFeatures(literatureTestSuiteResults, "litSuiteTestingFeatures", mult = graphScale)
cat("- done.\n")
# Add columns of information when plotting model performance
literatureTestSuiteResults[literatureTestSuiteResults$superiorModel == "Co-Kriging", "pch"] <- 17
literatureTestSuiteResults[literatureTestSuiteResults$superiorModel == "Co-Kriging", "col"] <- "green4"
literatureTestSuiteResults[literatureTestSuiteResults$superiorModel == "Co-Kriging", "legend"] <- "Co-Kriging"
literatureTestSuiteResults[literatureTestSuiteResults$superiorModel == "Tied", "pch"] <- 16
literatureTestSuiteResults[literatureTestSuiteResults$superiorModel == "Tied", "col"] <- "darkgoldenrod1"
literatureTestSuiteResults[literatureTestSuiteResults$superiorModel == "Tied", "legend"] <- "Tied"
literatureTestSuiteResults[literatureTestSuiteResults$superiorModel == "Kriging", "pch"] <- 15
literatureTestSuiteResults[literatureTestSuiteResults$superiorModel == "Kriging", "col"] <- "red"
literatureTestSuiteResults[literatureTestSuiteResults$superiorModel == "Kriging", "legend"] <- "Kriging"
# Plot performance graphs
cat("Plotting performance graphs ")
plotPerformance(literatureTestSuiteResults, "litSuiteTestingFeatures", mult = graphScale)
cat("- done.\n")

cat("Perform decision tree analysis:\n")
runDecisionTreeAnalysis(literatureTestSuiteResults, "literatureTestSuite")
print("ANALYSED LITERATURE SUITE EXPERIMENTS.")