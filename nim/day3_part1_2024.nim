
import strutils # For readFile, find, parseInt, isDigit
import os       # For file existence check (optional but good practice)

const INPUT_FILENAME = "input.txt"

proc parseNumber(data: string, startPos: var int): (int, bool) =
  ## Tries to parse a 1-3 digit number starting at startPos.
  ## Updates startPos to point after the parsed number.
  ## Returns (number, success_flag).
  let initialPos = startPos
  while startPos < data.len and data[startPos].isDigit():
    inc startPos

  let numStr = data[initialPos ..< startPos] # Exclusive end index
  let len = numStr.len
  if len > 0 and len <= 3:
    try:
      return (parseInt(numStr), true)
    except ValueError:
      # Should not happen if isDigit check is correct, but good to handle
      return (0, false)
  else:
    # Reset startPos if parsing failed due to length
    startPos = initialPos
    return (0, false)

proc main() =
  # Ensure input file exists
  if not fileExists(INPUT_FILENAME):
    echo "Error: Input file '", INPUT_FILENAME, "' not found."
    quit(1)

  var totalSum = 0
  let inputData = try:
                    readFile(INPUT_FILENAME)
                  except IOError as e:
                    echo "Error reading file '", INPUT_FILENAME, "': ", e.msg
                    quit(1)

  var currentIndex = 0
  while currentIndex < inputData.len:
    # 1. Find the start of a potential instruction
    let mulPos = inputData.find("mul(", currentIndex)
    if mulPos == -1: # No more "mul(" found
      break

    var parsePos = mulPos + 4 # Position right after "mul("

    # 2. Try parsing the first number (X)
    let (num1, success1) = parseNumber(inputData, parsePos)
    if not success1:
      # Failed to parse X, or pattern broken. Advance past 'm' of "mul("
      currentIndex = mulPos + 1
      continue

    # 3. Check for the comma
    if parsePos >= inputData.len or inputData[parsePos] != ',':
      # Missing or wrong character after X. Advance past 'm'.
      currentIndex = mulPos + 1
      continue
    inc parsePos # Move past the comma

    # 4. Try parsing the second number (Y)
    let (num2, success2) = parseNumber(inputData, parsePos)
    if not success2:
      # Failed to parse Y. Advance past 'm'.
      currentIndex = mulPos + 1
      continue

    # 5. Check for the closing parenthesis
    if parsePos >= inputData.len or inputData[parsePos] != ')':
      # Missing or wrong character after Y. Advance past 'm'.
      currentIndex = mulPos + 1
      continue

    # 6. Success! Found a valid mul(X,Y) instruction
    totalSum += num1 * num2

    # 7. Advance search past the current valid instruction ')'
    currentIndex = parsePos + 1

  # Print the final result to standard output
  echo totalSum

# Program entry point
when isMainModule:
  main()
