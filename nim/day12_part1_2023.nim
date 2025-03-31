
import std / [strutils, sequtils, tables, os]

# --- Core Logic with Memoization ---

# Define a type for the memoization cache key
type CacheKey = tuple[springIdx, groupIdx, currentGroupLen: int]
# Define the cache type mapping the key to the count of arrangements
type Cache = Table[CacheKey, int64] # Use int64 for potentially large counts

# Recursive function with memoization to count valid arrangements
proc countArrangements(springs: string, groups: seq[int], springIdx, groupIdx, currentGroupLen: int, cache: var Cache): int64 =
  # Create the key for the current state
  let key = (springIdx, groupIdx, currentGroupLen)
  # Return cached result if this state has been computed before
  if key in cache:
    return cache[key]

  # Base case: Reached the end of the springs string (including the padding '.')
  if springIdx == springs.len:
    # Check if we successfully matched all groups
    if groupIdx == groups.len and currentGroupLen == 0:
      # All groups matched, no current group being built - Valid arrangement
      return 1
    elif groupIdx == groups.len - 1 and currentGroupLen == groups[groupIdx]:
      # Last group was just completed at the very end - Valid arrangement
      return 1
    else:
      # Mismatch in groups or left in the middle of a group - Invalid arrangement
      return 0

  var total: int64 = 0
  let currentChar = springs[springIdx]

  # --- Explore possibilities based on the current character ---

  # Possibility 1: Treat current character as OPERATIONAL ('.')
  # This happens if the character is '.' or '?'.
  if currentChar == '.' or currentChar == '?':
    if currentGroupLen == 0:
      # Not currently building a group, just move to the next spring
      total += countArrangements(springs, groups, springIdx + 1, groupIdx, 0, cache)
    else:
      # Was building a group, this '.' potentially ends it.
      # Check if the completed group matches the expected size.
      if groupIdx < groups.len and currentGroupLen == groups[groupIdx]:
        # Group size matches, move to the next spring and the next group index
        total += countArrangements(springs, groups, springIdx + 1, groupIdx + 1, 0, cache)
      # else: The group size doesn't match, this path is invalid (adds 0).

  # Possibility 2: Treat current character as DAMAGED ('#')
  # This happens if the character is '#' or '?'.
  if currentChar == '#' or currentChar == '?':
    # Check if we are still within the expected groups and the current group hasn't exceeded its expected size
    if groupIdx < groups.len and currentGroupLen < groups[groupIdx]:
      # Continue building the current group
      total += countArrangements(springs, groups, springIdx + 1, groupIdx, currentGroupLen + 1, cache)
    # else: Too many '#' for the current group or no more groups expected, this path is invalid (adds 0).

  # Store the result for this state in the cache before returning
  cache[key] = total
  return total

# --- Main Program Logic ---

proc solve(filename: string): int64 =
  ## Reads the input file, processes each line, and returns the total count.
  var totalArrangements: int64 = 0
  let fileContent = readFile(filename)

  for line in fileContent.strip.splitLines:
    if line.len == 0: continue # Skip empty lines

    # Split line into springs pattern and group definition
    let parts = line.split(' ')
    if parts.len != 2:
      stderr.writeLine "Warning: Skipping malformed line: ", line
      continue

    let springsRaw = parts[0]
    let groupsStr = parts[1]

    # Parse the group sizes
    let groups = try:
                   groupsStr.split(',').map(parseInt)
                 except ValueError:
                   stderr.writeLine "Warning: Skipping line with invalid group numbers: ", line
                   continue

    # --- Optimization: Add a '.' padding to the end ---
    # This simplifies the base case handling, ensuring any trailing group
    # is properly terminated and checked.
    let springsPadded = springsRaw & "."

    # Initialize the memoization cache for each line
    var cache = initTable[CacheKey, int64]()

    # Calculate arrangements for the current line and add to total
    totalArrangements += countArrangements(springsPadded, groups, 0, 0, 0, cache)

  return totalArrangements

# --- Entry Point ---
when isMainModule:
  # Define the input filename
  let filename = "input.txt"

  # Check if the input file exists
  if not fileExists(filename):
    stderr.writeLine "Error: Input file '", filename, "' not found."
  else:
    # Solve the problem and print the result
    let result = solve(filename)
    echo result
