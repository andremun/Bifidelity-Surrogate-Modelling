#!/bin/bash
#SBATCH --job-name=newInstancesFeatures
#SBATCH --time=24:00:00
#SBATCH --partition=snowy
#SBATCH --mem=5G
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --array=1-810

# Load the required modules
module load gcccore/10.2.0
module load cmake/3.18.4
module load eigen/3.3.8
# Move into folder and run, each line should be very quick so do 100 per job
cd ../cpp_code
./main newInstancesFeatures ${SLURM_ARRAY_TASK_ID} 100





