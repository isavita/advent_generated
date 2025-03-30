
import std/[strutils, sequtils, sets, tables, os]

# Global variables for memoization table and available patterns
# Using globals simplifies the recursive function signature
var
  memo: Table[string, int64]       # Memoization cache: design suffix -> number of ways
  patterns: HashSet[string]   # Set of available towel patterns for fast lookup

# Recursive function with memoization to count the ways a design can be formed
proc countWays(design: string): int64 =
  # Base case: An empty design can be formed in exactly one way (by using no towels)
  if design.len == 0:
    return 1

  # Check if the result for this design suffix is already computed
  if memo.contains(design):
    return memo[design]

  # Initialize count for the current design suffix
  var currentWays: int64 = 0

  # Iterate through all available towel patterns
  for pattern in patterns:
    # Check if the current pattern is a prefix of the remaining design
    if design.startsWith(pattern):
      # If it matches, recursively call countWays for the rest of the design string
      # The slice design[pattern.len .. ^1] gets the substring after the matched pattern
      # Note: Nim's ^1 means the last index
      let remainingDesign = design[pattern.len .. ^1]
      currentWays += countWays(remainingDesign) # Add ways from the subproblem

  # Store the computed result in the memoization table before returning
  memo[design] = currentWays
  return currentWays

# Main procedure acting as the entry point
proc main() =
  # Check if input file exists
  if not fileExists("input.txt"):
    echo "Error: input.txt not found."
    quit(1)

  # Open the input file
  let f = open("input.txt")
  defer: f.close() # Ensure the file is closed even if errors occur

  # Read the first line containing towel patterns
  let patternsLine = f.readLine()
  # Split by comma, strip whitespace from each pattern, and store in the HashSet
  patterns = patternsLine.split(',').mapIt(it.strip).toHashSet()

  # Read and discard the blank line separating patterns and designs
  discard f.readLine()

  # Initialize counters for Part 1 and Part 2
  var possibleDesignsCount = 0
  var totalWaysSum: int64 = 0

  # Process each design line
  for design in f.lines():
    let trimmedDesign = design.strip() # Remove potential leading/trailing whitespace
    if trimmedDesign.len == 0: continue # Skip empty lines if any

    # **Crucial:** Reset the memoization table for each new design
    # Each design is an independent problem
    memo = initTable[string, int64]()

    # Calculate the number of ways to form the current design
    let ways = countWays(trimmedDesign)

    # Part 1: If ways > 0, the design is possible
    if ways > 0:
      possibleDesignsCount += 1

    # Part 2: Add the number of ways for this design to the total sum
    totalWaysSum += ways

  # Print the results
  echo "Part 1 (Possible Designs): ", possibleDesignsCount
  echo "Part 2 (Total Ways): ", totalWaysSum

# Execute the main procedure
when isMainModule:
  main()
