
import strutils, sets, math

proc calculateCardPoints(line: string): int =
  ## Calculates the points for a single scratchcard line.

  # Find the separators
  let colonPos = line.find(':')
  let pipePos = line.find('|')

  if colonPos == -1 or pipePos == -1 or colonPos >= pipePos:
    # Basic validation for line structure
    echo "Warning: Skipping malformed line: ", line
    return 0

  # Extract number strings, stripping whitespace
  let winningStr = line[colonPos + 1 .. pipePos - 1].strip()
  let yourStr = line[pipePos + 1 .. ^1].strip()

  # Parse winning numbers into a HashSet for efficient lookup
  var winningNumbers = initHashSet[int]()
  for numStr in winningStr.splitWhitespace():
    if numStr.len > 0: # Ensure we don't parse empty strings
      try:
        winningNumbers.incl parseInt(numStr)
      except ValueError:
        echo "Warning: Invalid winning number format: ", numStr
        # Decide how to handle - skip number, skip line, or error out
        # For this challenge, skipping the number seems reasonable.

  # Parse your numbers and count matches
  var matchCount = 0
  for numStr in yourStr.splitWhitespace():
    if numStr.len > 0: # Ensure we don't parse empty strings
      try:
        let yourNum = parseInt(numStr)
        if yourNum in winningNumbers:
          matchCount += 1
      except ValueError:
        echo "Warning: Invalid 'your' number format: ", numStr
        # Skipping the number

  # Calculate points based on matches
  if matchCount == 0:
    return 0
  else:
    # 2^(matchCount - 1) can be efficiently calculated using bit shifts
    # 1 shl (matchCount - 1)
    # Example:
    # matchCount = 1 -> 1 shl 0 = 1
    # matchCount = 2 -> 1 shl 1 = 2
    # matchCount = 3 -> 1 shl 2 = 4
    # matchCount = 4 -> 1 shl 3 = 8
    return 1 shl (matchCount - 1)

proc main() =
  ## Reads input.txt, calculates total points, and prints the result.
  var totalPoints = 0
  let filename = "input.txt"

  try:
    for line in lines(filename):
      if line.len > 0: # Process non-empty lines
        totalPoints += calculateCardPoints(line)
  except IOError:
    echo "Error: Could not read file '", filename, "'"
    quit(1) # Exit with an error code

  echo totalPoints

# Standard Nim entry point check
when isMainModule:
  main()
