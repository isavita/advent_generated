awk '{print $0+0}' input.txt | sort -n | awk '{diff = $0 - prev; count[diff]++; prev = $0} END {print count[1] * (count[3]+1)}'