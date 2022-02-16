# Script which runs the literature tests in sequence.
# Note: Doing this is really not recommended as it will take a very long time!
# Providing this to show how the code works more than anything else.

#!/bin/bash
# There are a total of 906 jobs, run them sequentially. The code knows what 
# to read and where to store it.
cd cpp_code
for i in {1..1}
do
  echo "Experiment line $i/906"
  ./main litSuiteTesting $i
done






