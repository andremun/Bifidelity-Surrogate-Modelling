# Script which runs the model performance on the newly created instances.
# Note: Doing this is really not recommended as it will take a very long time!
# Providing this to show how the code works more than anything else.

#!/bin/bash
# There are a total of 5968 jobs, run them sequentially. The code knows what 
# to read and where to store it.
# Added parameter 6 reads and executes 6 lines, do so to reduce number of 
# files storing results.
cd cpp_code
for i in {1..995}
do
  echo "Experiment line $i/995"
  ./main newInstancesTesting $i 6
done






