
import strutils
import os
import options

# --- Constants ---
const INPUT_FILENAME = "input.txt"

# --- Helper Procedures ---

proc tryParseBoundedInt(s: string, startIdx: var int, maxLen: int = 3): Option[int] =
  ## Attempts to parse a positive integer of at most maxLen digits starting at startIdx.
  ## Updates startIdx to point *after* the parsed integer on success.
  ## Returns None if parsing fails or no digits are found.

  var currentIdx = startIdx
  var numStr = ""

  # Read up to maxLen digits
  for _ in 1..maxLen:
    if currentIdx < s.len and s[currentIdx].isDigit():
      numStr.add(s[currentIdx])
      inc currentIdx
    else:
      break # Stop if non-digit or end of string

  # Check if any digits were actually read
  if numStr.len == 0:
    return none(int)

  # Try to parse the collected digits
  try:
    let value = parseInt(numStr)
    startIdx = currentIdx # Update original index only on successful parse
    return some(value)
  except ValueError:
    # This shouldn't happen if isDigit worked correctly, but good practice
    return none(int)


# --- Main Logic ---

proc solve(): int =
  let content = try:
                  readFile(INPUT_FILENAME)
                except IOError:
                  echo "Error: Could not read file '", INPUT_FILENAME, "'"
                  quit(1)

  var totalSum = 0
  var i = 0
  let n = content.len
  var mulsEnabled = true # Start with multiplications enabled (Part 2)

  while i < n:
    # Check for "mul(" instruction (length 4)
    if i + 4 <= n and content[i ..< i + 4] == "mul(":
      var currentPos = i + 4 # Start parsing after "mul("
      let initialPos = currentPos # Store position before parsing numbers

      # Try parse first number (X)
      let num1Opt = tryParseBoundedInt(content, currentPos)
      if num1Opt.isSome:
        let num1 = num1Opt.get()

        # Check for comma
        if currentPos < n and content[currentPos] == ',':
          inc currentPos # Move past comma

          # Try parse second number (Y)
          let num2Opt = tryParseBoundedInt(content, currentPos)
          if num2Opt.isSome:
            let num2 = num2Opt.get()

            # Check for closing parenthesis
            if currentPos < n and content[currentPos] == ')':
              # Valid mul instruction found!
              if mulsEnabled: # Check if enabled (Part 2)
                totalSum += num1 * num2

              # Advance main index past the processed instruction
              i = currentPos + 1
              continue # Skip the default increment at the end of the loop
            # else: Invalid - missing ')'
          # else: Invalid - second number parse failed
        # else: Invalid - missing ','
      # else: Invalid - first number parse failed

      # If any part of the mul parse failed after "mul(",
      # we still need to advance the main index.
      # We advance by one to check the next character, as the failed
      # instruction might start slightly later (e.g., "mull(1,2)").
      i += 1

    # Check for "do(" instruction (length 3)
    elif i + 3 <= n and content[i ..< i + 3] == "do(":
      if i + 3 < n and content[i + 3] == ')':
        # Valid do() instruction found
        mulsEnabled = true
        i += 4 # Advance past "do()"
        continue
      else:
        # Malformed do - just advance index by 1
        i += 1

    # Check for "don't(" instruction (length 6)
    elif i + 6 <= n and content[i ..< i + 6] == "don't(":
      if i + 6 < n and content[i + 6] == ')':
        # Valid don't() instruction found
        mulsEnabled = false
        i += 7 # Advance past "don't()"
        continue
      else:
        # Malformed don't - just advance index by 1
        i += 1

    else:
      # No instruction matched at this position, advance index by 1
      i += 1

  return totalSum

# --- Entry Point ---
when isMainModule:
  let result = solve()
  echo result
