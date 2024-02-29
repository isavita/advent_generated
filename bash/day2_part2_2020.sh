#!/bin/bash

# Define the file to read from
inputFile="input.txt"

# Initialize the count of valid passwords
validCount=0

# Read each line of the file
while IFS= read -r line || [[ -n "$line" ]]; do
  # Extract the policy and the password from the line
  policy=${line%%:*}
  password=${line#*: }
  
  # Extract min, max, and the character from the policy
  IFS=' -' read -r min max char <<< "$policy"
  
  # Extract the characters at positions min and max from the password
  charAtMin=${password:$((min-1)):1}
  charAtMax=${password:$((max-1)):1}
  
  # Check if exactly one of the positions contains the character
  if [[ "$charAtMin" == "$char" && "$charAtMax" != "$char" ]] || [[ "$charAtMin" != "$char" && "$charAtMax" == "$char" ]]; then
    ((validCount++))
  fi
done < "$inputFile"

# Print the count of valid passwords
echo "$validCount"