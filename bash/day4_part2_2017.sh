
#!/bin/bash

function sort_string {
  echo "$1" | grep -o . | sort | tr -d '\n'
}

valid_count=0

while IFS= read -r line; do
  valid=true
  word_set=()
  for word in $line; do
    sorted_word=$(sort_string "$word")
    if [[ " ${word_set[@]} " =~ " ${sorted_word} " ]]; then
      valid=false
      break
    fi
    word_set+=("$sorted_word")
  done
  if $valid; then
    ((valid_count++))
  fi
done < "input.txt"

echo "$valid_count"
