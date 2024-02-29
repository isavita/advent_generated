#!/bin/bash

# Read input from file
while IFS= read -r line; do
  parts=($(echo $line | tr " " "\n"))

  from=${parts[0]}
  toNodes=${parts[2]}

  IFS=, read -r -a toNodesArray <<< "$toNodes"

  for toNode in "${toNodesArray[@]}"
  do
    echo $from "<->" $toNode
  done
done < "input.txt"

# Perform DFS
count=0
while IFS= read -r line; do
  parts=($(echo $line | tr " " "\n"))

  from=${parts[0]}
  toNodes=${parts[2]}

  IFS=, read -r -a toNodesArray <<< "$toNodes"

  for toNode in "${toNodesArray[@]}"
  do
    echo "Visited $from and $toNode"
    ((count++))
  done
done < "input.txt"

echo $count