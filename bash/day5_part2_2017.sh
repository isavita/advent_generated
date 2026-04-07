
#!/bin/bash
awk '{m[NR]=$1} END {p=1; while(p>=1 && p<=NR){o=m[p]; m[p]+=(o>=3?-1:1); p+=o; s++} print s}' input.txt
