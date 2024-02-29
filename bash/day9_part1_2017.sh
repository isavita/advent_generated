#!/bin/bash

# Initialize Variables
score=0
depth=0
inGarbage=0
cancelNext=0

# Read and Process Stream from "input.txt"
while IFS= read -r -n1 ch; do
  if [[ $cancelNext -eq 1 ]]; then
    cancelNext=0
    continue
  fi

  if [[ $inGarbage -eq 1 ]]; then
    if [[ $ch == '!' ]]; then
      cancelNext=1
    elif [[ $ch == '>' ]]; then
      inGarbage=0
    fi
  else
    case $ch in
      '{')
        ((depth++))
        ;;
      '}')
        ((score+=depth))
        ((depth--))
        ;;
      '<')
        inGarbage=1
        ;;
    esac
  fi
done < "input.txt"

# Print Score
echo $score