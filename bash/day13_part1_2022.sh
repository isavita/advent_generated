
#!/usr/bin/env bash

# --- Day 13: Distress Signal ---
#
# Compares pairs of packets (JSON-like lists/integers) according to specific rules
# and calculates the sum of the 1-based indices of pairs that are in the correct order.

# Strict mode
set -euo pipefail

# Define the jq comparison function globally for clarity and reuse.
# This function implements the comparison logic described in the problem.
# It returns:
#   -1 if left < right (correct order)
#    0 if left == right (continue)
#    1 if left > right (wrong order)
readonly JQ_CMP_FUNC='
# Recursive comparison function
def compare(left; right):
  # Case 1: Both are numbers
  if (left|type) == "number" and (right|type) == "number" then
    if left < right then -1
    elif left > right then 1
    else 0 end
  # Case 2: Both are arrays
  elif (left|type) == "array" and (right|type) == "array" then
    # Iterate through elements up to the length of the shorter list + 1
    reduce range(0; ([left|length, right|length]|max) + 1) as $i (0; # 0 = undecided
      # If a decision has been made in a previous iteration, keep it
      if . != 0 then .
      # Check if left list ran out
      elif $i >= (left|length) and $i < (right|length) then -1 # Correct order
      # Check if right list ran out
      elif $i < (left|length) and $i >= (right|length) then 1 # Wrong order
      # Check if both lists ran out at the same time
      elif $i >= (left|length) and $i >= (right|length) then 0 # Undecided (equal)
      # Otherwise, compare elements at index $i recursively
      else compare(left[$i]; right[$i])
      end
    )
  # Case 3: Mixed types - convert integer to list
  elif (left|type) == "number" and (right|type) == "array" then
    compare([left]; right)
  elif (left|type) == "array" and (right|type) == "number" then
    compare(left; [right])
  # Should not happen with valid input, but return 0 (equal) defensively
  else
    0
  end;

# Call the comparison function with the input arguments $left_packet and $right_packet
# These arguments will be passed using --argjson from the bash script.
compare($left_packet; $right_packet)
'

# Main function to orchestrate the process
main() {
  local input_file="input.txt"
  local index=0
  local correct_order_index_sum=0
  local left_packet=""
  local right_packet=""
  local line_count=0

  # Check if input file exists
  if [[ ! -f "$input_file" ]]; then
    echo "Error: Input file '$input_file' not found." >&2
    exit 1
  fi

  # Check if jq is installed
  if ! command -v jq &> /dev/null; then
    echo "Error: 'jq' command is required but not found. Please install jq." >&2
    exit 1
  fi

  # Read the input file line by line
  while IFS= read -r line || [[ -n "$line" ]]; do # Handle files not ending with newline
    line_count=$((line_count + 1))

    if [[ -z "$line" ]]; then
      # Blank line indicates the end of a pair
      # Reset packets for the next pair (though not strictly necessary)
      left_packet=""
      right_packet=""
      continue
    fi

    if [[ -z "$left_packet" ]]; then
      # This is the first line of a pair
      left_packet="$line"
    else
      # This is the second line of a pair
      right_packet="$line"
      index=$((index + 1)) # Increment pair index

      # Use jq to compare the two packets
      local comparison_result
      # Pass packets as JSON arguments to jq using --argjson
      comparison_result=$(jq -n --argjson left_packet "$left_packet" \
                                --argjson right_packet "$right_packet" \
                                "$JQ_CMP_FUNC")

      # Check the comparison result
      # -1 means left < right (correct order)
      if [[ "$comparison_result" -le 0 ]]; then # Problem states equal also means "continue checking", implying not "wrong order" yet. The crucial part is if left > right. Result <=0 covers left < right and left == right for the overall packet comparison. A top-level result of 0 according to rules means left didn't come after right. The examples confirm <=0 is the condition. Pair 4 is equal until left runs out => -1. Pair 7 [[[]]] vs [[]] -> compare [[]] vs [] -> right runs out => 1.
        if [[ "$comparison_result" -lt 0 ]]; then # Specifically check for -1 (left < right)
            correct_order_index_sum=$((correct_order_index_sum + index))
            # echo "Pair $index: Correct Order ($left_packet vs $right_packet)" # Uncomment for debugging
        # else
            # echo "Pair $index: Equal Order ($left_packet vs $right_packet)" # Uncomment for debugging
            # Equal is considered NOT the wrong order, so we don't add to sum, but don't exclude based on this check alone if using -le 0
            # For part 1, only index sum of strictly ordered pairs (-1 result) is needed.
        fi
      # else
        # echo "Pair $index: Wrong Order ($left_packet vs $right_packet)" # Uncomment for debugging
      fi

      # Reset for the next potential pair after processing
      left_packet=""
      right_packet=""

    fi
  done < "$input_file"

  # Final check: Ensure the last read lines formed a complete pair if file doesn't end with blank line
   if [[ -n "$left_packet" && -z "$right_packet" ]]; then
       echo "Warning: Input file ends with an incomplete pair (only left packet found)." >&2
   fi


  # Print the final sum
  echo "$correct_order_index_sum"
}

# Execute the main function
main
