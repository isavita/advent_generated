
#!/bin/bash

# --- Day 3: Mull It Over ---
#
# Reads corrupted memory from input.txt, finds valid 'mul(X,Y)' instructions,
# calculates X * Y for each, and prints the sum of all results.
# Valid X and Y are 1-3 digit positive integers.
# The pattern must be exactly mul(X,Y) with no extra characters.

# Function to process the input file and calculate the sum
main() {
  local input_file="input.txt"
  local total=0
  local line
  local segment
  local x y result

  # Check if input file exists and is readable
  if [[ ! -r "$input_file" ]]; then
    echo "Error: Cannot read input file '$input_file'" >&2
    return 1
  fi

  # --- Option 1: Using grep -o and a Bash loop (Readable & Common) ---
  # Extract all valid 'mul(X,Y)' segments using grep -o.
  # The regex ensures:
  # - Starts exactly with 'mul('
  # - Followed by 1 to 3 digits (captured as group 1)
  # - Followed by a comma ','
  # - Followed by 1 to 3 digits (captured as group 2)
  # - Ends exactly with ')'
  # We use process substitution <(...) to feed the grep output line by line
  # into the while loop without needing an intermediate file or variable holding
  # the entire output (more memory efficient for large inputs).
  # The regex uses Basic Regular Expressions (BRE) syntax suitable for standard grep.
  
  # Read the entire file content into a variable first. This avoids issues
  # if input.txt doesn't end with a newline, which could affect `read`.
  # It's generally safe for typical competitive programming input sizes.
  local content
  content=$(<"$input_file")

  # Use grep with Perl-compatible regexes (-P) for easier capture group handling,
  # though standard grep -o works too with more parsing in the loop.
  # The -o flag prints only the matched parts, each on a new line.
  while IFS= read -r segment; do
      # Extract numbers X and Y from the segment 'mul(X,Y)'
      # Using Bash Parameter Expansion for efficiency:
      local temp="${segment#mul(}" # Remove 'mul(' prefix -> "X,Y)"
      temp="${temp%)}"             # Remove ')' suffix   -> "X,Y"
      x="${temp%,*}"               # Remove ',Y' suffix  -> "X"
      y="${temp#*,}"               # Remove 'X,' prefix  -> "Y"

      # Calculate the product and add to the total
      # Using Bash arithmetic expansion `((...))`
      result=$((x * y))
      total=$((total + result))
  done < <(grep -o 'mul([0-9]\{1,3\},[0-9]\{1,3\})' <<< "$content")
  # Note: Using 'mul([0-9]{1,3},[0-9]{1,3})' with grep -E would also work.
  # Note: Using <<< "$content" feeds the variable content to grep's stdin.

  # Print the final sum
  echo "$total"

  # --- Option 2: Using awk (Concise & Powerful for text processing) ---
  # This alternative performs the matching, extraction, and summation
  # within a single awk command. It might be considered more efficient
  # as it avoids the overhead of the Bash loop and multiple grep/shell processes.
  # Requires gawk (GNU awk) for the third argument to match() to capture groups.

  # gawk '
  # {
  #   offset = 1 # Start search from position 1
  #   while (match(substr($0, offset), /mul\(([0-9]{1,3}),([0-9]{1,3})\)/, arr)) {
  #     # arr[1] holds X, arr[2] holds Y
  #     x = arr[1]
  #     y = arr[2]
  #     total += x * y
  #     # Advance offset past the current match to find the next one
  #     offset += RSTART + RLENGTH - 1
  #   }
  # }
  # END { print total + 0 } # Print total (add 0 to ensure numeric output even if total is 0)
  # ' "$input_file"

  return 0
}

# Call the main function
main
