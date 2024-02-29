
#!/bin/bash

totalElves=$(cat input.txt)
highestPowerOfTwo=1

while [ $((highestPowerOfTwo*2)) -le $totalElves ]; do
  highestPowerOfTwo=$((highestPowerOfTwo*2))
done

echo $(( (totalElves-highestPowerOfTwo)*2 + 1 ))
