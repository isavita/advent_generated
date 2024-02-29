
#!/bin/bash

declare -a visitedHouses
declare -i index=0
x=0
y=0

while IFS= read -r -n1 dir; do
  case $dir in
    "^") y=$((y+1)) ;; # Move north
    "v") y=$((y-1)) ;; # Move south
    ">") x=$((x+1)) ;; # Move east
    "<") x=$((x-1)) ;; # Move west
  esac
  key="$x,$y"
  visitedHouses[index]="$key"
  ((index++))
done < "input.txt"

# Remove duplicates
visitedHouses=($(printf '%s\n' "${visitedHouses[@]}" | sort -u))

echo "${#visitedHouses[@]}"
