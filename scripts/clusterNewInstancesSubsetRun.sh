#!/bin/bash
#SBATCH --job-name=newInstancesSubset
#SBATCH --time=24:00:00
#SBATCH --partition=snowy
#SBATCH --mem=5G
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --array=1-995

# Load the required modules
module load gcccore/10.2.0
module load cmake/3.18.4
module load eigen/3.3.8
# Move into folder and run, each have a total of 5968 so
# have 995 runs with 6 lines each.
cd ../cpp_code
./main newInstancesTesting ${SLURM_ARRAY_TASK_ID} 6





