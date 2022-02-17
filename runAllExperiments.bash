# This is the main bash file from which all scripts are run
# Note that running this bash script from start to finish will
# generate all of the data and plots, but will take a very very long time.
# It is instead recommended that when large experiments need to be run,
# a computer cluster is used. The times where this is recommended are
# highlighted below using ***, with a sample bash file which specifies
# the cluster run given. It is strongly recommended you run sections
# of this script on your local computer, and others on a cluster.


# REQUIREMENTS
# For this script to execute, the following must be installed in your local machine:
#		- gcc: Although any C++ compiler should work, make sure if using a different compiler
#				to modify the Makefile files inside cpp_code.
#
#		- cmake
#
#		- Eigen: For details consult https://eigen.tuxfamily.org/dox/GettingStarted.html
#					Make sure you modify the path to eigen inside Makefile.local to match
#					that of your download location.
#
#		- R: Also certain libraries are needed: stringr, ggplot2, gridExtra, egg, rpart, tibble, bitops and rattle
#	


# RUNNING ALL EXPERIMENTS

# Compile C++ code
cd cpp_code
make local
cd ..

# Create script for literature suite experimental run.
Rscript R_code/createLiteratureExperimentalScript.R

# *** Run literature suite experimental run. ***
# The next line runs everything locally:
# bash scripts/localLitRun.sh
# To run on cluster (recommended), modify the script below for cluster run.
# Note the same folder structure of the whole project is required, including
# the folders scripts/, data/clusterResults/, and data/runScripts containing 
# the file "litSuiteTesting.txt"
# scripts/clusterLitRun.sh

# Run analysis of literature suite experimental results.
Rscript R_code/analyseLiteratureExperiments.R

# Create script which analyses features of 81000 new instances.
Rscript R_code/createNewInstancesFeaturesScript.R

# *** Analyse features of the new instances. ***
# The next line runs everything locally:
# bash scripts/localNewInstancesFeaturesRun.sh
# To run on cluster (recommended), modify the script below for cluster run.
# Note the same folder structure of the whole project is required, including
# the folders scripts/, data/clusterResults/, and data/runScripts containing 
# the file "newInstancesFeatures.txt"
# scripts/clusterNewInstancesFeaturesRun.sh

# Run analysis of new instances, choose subset and create script
# for an experimental run on this subset.
Rscript R_code/createNewInstancesExperimentalScript.R

# *** Run new suite experimental run. ***
# The next line runs everything locally:
# bash scripts/localNewInstancesSubsetRun.sh
# To run on cluster (recommended), modify the script below for cluster run.
# Note the same folder structure of the whole project is required, including
# the folders scripts/, data/clusterResults/, and data/runScripts containing 
# the file "newInstancesTesting.txt"
# scripts/clusterNewInstancesSubsetRun.sh

# Run analysis of new suite experimental results, known as 
# augmented test suite.
# Rscript R_code/analyseAugmentedExperiments.R



