# Script which runs the feature analysis on the newly created instances.
# Note: Doing this is really not recommended as it will take a very long time!
# Providing this to show how the code works more than anything else.

#!/bin/bash
# There are a total of 81000 jobs, run them sequentially. The code knows what 
# to read and where to store it.
# Added parameter 100 reads and executes 100 lines, do so to reduce number of 
# files storing results.
cd cpp_code
for i in {1..810}
do
  echo "Experiment line $i/810"
  ./main newInstancesFeatures $i 100
done






