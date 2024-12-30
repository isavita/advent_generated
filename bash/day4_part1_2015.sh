
#!/bin/bash

secret_key=$(tr -d '\n' < input.txt)
number=0
while true; do
  hash=$(echo -n "$secret_key$number" | md5sum | awk '{print $1}')
  if [[ "$hash" == 00000* ]]; then
    echo "$number"
    break
  fi
  ((number++))
done
