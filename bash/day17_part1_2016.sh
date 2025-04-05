
#!/bin/bash

shopt -s expand_aliases

# Alias md5sum for potential platform differences (macOS vs Linux)
if command -v md5 > /dev/null 2>&1; then
  alias md5sum='md5 -q' # macOS
elif command -v md5sum > /dev/null 2>&1; then
  alias md5sum='md5sum' # Linux
else
  echo "Error: md5sum or md5 command not found." >&2
  exit 1
fi

get_hash() {
  local input_string="$1"
  local hash
  hash=$(echo -n "$input_string" | md5sum)
  # Handle potential differences in md5sum output format (e.g., ' -' suffix)
  echo "${hash%% *}"
}

find_shortest_path() {
  local passcode="$1"
  local -a queue
  local directions=("U" "D" "L" "R")
  local dx=(0 0 -1 1)
  local dy=(-1 1 0 0)

  # Queue stores "x,y,path"
  queue=("0,0,")

  while (( ${#queue[@]} > 0 )); do
    # Dequeue
    local current_state="${queue[0]}"
    # More efficient slicing for Bash 4.3+ might be possible, but this is compatible
    queue=("${queue[@]:1}")

    # Parse state
    IFS=',' read -r x y path <<< "$current_state"

    # Goal check
    if (( x == 3 && y == 3 )); then
      echo "$path"
      return 0
    fi

    # Get hash
    local hash_input="$passcode$path"
    local hash_str
    hash_str=$(get_hash "$hash_input")

    # Explore neighbours
    local i char direction new_x new_y
    for i in 0 1 2 3; do
      char="${hash_str:i:1}"
      # Check if door is open ('b' through 'f')
      if [[ "$char" =~ [b-f] ]]; then
        direction="${directions[i]}"
        new_x=$((x + dx[i]))
        new_y=$((y + dy[i]))

        # Check bounds (0 <= new_x < 4 and 0 <= new_y < 4)
        if (( new_x >= 0 && new_x < 4 && new_y >= 0 && new_y < 4 )); then
          # Enqueue new state
          queue+=("$new_x,$new_y,$path$direction")
        fi
      fi
    done
  done

  echo "Error: No path found" >&2
  return 1
}

main() {
  if [[ ! -f "input.txt" ]]; then
    echo "Error: input.txt not found." >&2
    exit 1
  fi
  # Read the entire file content, trim trailing newline
  local passcode
  passcode=$(< input.txt)
  passcode="${passcode%$'\n'}" # Ensure trailing newline is removed if present

  if [[ -z "$passcode" ]]; then
    echo "Error: input.txt is empty." >&2
    exit 1
  fi

  find_shortest_path "$passcode"
}

main "$@"
