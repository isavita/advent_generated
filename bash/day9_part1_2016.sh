#!/bin/bash

input=$(<input.txt)
length=0

while [[ $input =~ \(([0-9]+)x([0-9]+)\) ]]; do
  before=${input%%\(*}
  length=$((length + ${#before}))
  input=${input#*$before}
  
  charCount=${BASH_REMATCH[1]}
  repeatCount=${BASH_REMATCH[2]}
  
  input=${input#*\)}
  toRepeat=${input:0:charCount}
  input=${input:charCount}
  
  length=$((length + charCount * repeatCount))
done

length=$((length + ${#input}))

echo $length