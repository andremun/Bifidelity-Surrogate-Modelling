#!/bin/bash
#SBATCH --job-name=litFullAnalysis
#SBATCH --time=24:00:00
#SBATCH --partition=snowy
#SBATCH --mem=20G
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --array=1-1453

# Load the required modules
module load gcccore/10.2.0
module load cmake/3.18.4
module load eigen/3.3.8
# Move into folder and run
cd ../cpp_code
# Call program with varying array (code knows how to deal with it)
# This one has 832 jobs
#./main modelPerformanceSetup ${SLURM_ARRAY_TASK_ID}
# This one has 848 jobs
#./main toalReproductionSetup ${SLURM_ARRAY_TASK_ID}
# This one has 926 jobs
#./main modelAccuracyBudget5d ${SLURM_ARRAY_TASK_ID}
# This is file with 86400 jobs
# Splitting it up into batches of 202 jobs with new call
# Will require 999 calls
#./main COCOinstanceFeatures ${SLURM_ARRAY_TASK_ID} 202
# This one has 1354 jobs
#./main COCOinterestingFunctionsBudget5 ${SLURM_ARRAY_TASK_ID} 
# This one has 20256 jobs
#./main litModelAccuracy ${SLURM_ARRAY_TASK_ID} 
# This one has 2266 jobs but it is really fast, so run batches of 100
#./main allFunctionsFeatures ${SLURM_ARRAY_TASK_ID} 100
# Rerun of the features of instances, has 720000 = 720*1000
#./main COCOinstanceFeaturesNoise1-2 ${SLURM_ARRAY_TASK_ID} 720
#./main COCOinstanceFeaturesNoise1-2reduced ${SLURM_ARRAY_TASK_ID} 1000
#./main COCOinstanceFeaturesNew ${SLURM_ARRAY_TASK_ID} 86

# Minitest with 120 lines
#./main miniTest ${SLURM_ARRAY_TASK_ID}

# Get sample from instances to analyse features
./main allFunctionsFeaturesRepeated ${SLURM_ARRAY_TASK_ID}





