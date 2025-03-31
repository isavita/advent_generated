
import strutils, sequtils, parseutils, tables, os, hashes

# Using BiggestInt as counts can get very large in Part 2
type Count = BiggestInt 
type Cache = Table[(int, int), Count] # (spring index, group index) -> count

# Recursive function with memoization to count valid arrangements
proc countArrangements(springs: string, groups: seq[int], springIdx, groupIdx: int, memo: var Cache): Count =
  # Check cache first
  let cacheKey = (springIdx, groupIdx)
  if memo.contains(cacheKey):
    return memo[cacheKey]

  # Base case 1: Reached the end of groups
  if groupIdx == groups.len:
    # Check if any remaining springs are '#', which would be invalid
    for i in springIdx ..< springs.len:
      if springs[i] == '#':
        memo[cacheKey] = 0 # Invalid arrangement
        return 0
    memo[cacheKey] = 1 # Valid arrangement (all remaining are '.' or '?')
    return 1

  # Base case 2: Reached the end of springs, but still have groups left
  if springIdx >= springs.len:
    memo[cacheKey] = 0 # Not enough springs to satisfy remaining groups
    return 0

  var result: Count = 0
  let currentGroupLen = groups[groupIdx]
  let currentSpring = springs[springIdx]

  # --- Try treating the current spring as '.' (or if it actually is '.') ---
  if currentSpring == '.' or currentSpring == '?':
    result += countArrangements(springs, groups, springIdx + 1, groupIdx, memo)

  # --- Try treating the current spring as '#' (or if it actually is '#') ---
  if currentSpring == '#' or currentSpring == '?':
    # Check if we can place the current group here
    var canPlaceGroup = true
    
    # 1. Check if there are enough springs left for the group
    if springIdx + currentGroupLen > springs.len:
      canPlaceGroup = false
    else:
      # 2. Check if all springs in the potential group range are '#' or '?'
      for i in springIdx ..< (springIdx + currentGroupLen):
        if springs[i] == '.':
          canPlaceGroup = false
          break
      
      # 3. Check if the spring immediately after the group is '.' or '?' (or end of string)
      if canPlaceGroup and (springIdx + currentGroupLen < springs.len):
          if springs[springIdx + currentGroupLen] == '#':
              canPlaceGroup = false

    # If the group can be placed here:
    if canPlaceGroup:
      # Calculate the next spring index: skip the group and the mandatory separator '.'
      let nextSpringIdx = springIdx + currentGroupLen + 1 
      result += countArrangements(springs, groups, nextSpringIdx, groupIdx + 1, memo)

  # Store result in cache before returning
  memo[cacheKey] = result
  return result

# Function to solve for a given set of lines and unfold factor
proc solve(lines: seq[string], unfoldFactor: int): Count =
  var totalArrangements: Count = 0

  for line in lines:
    if line.len == 0: continue # Skip empty lines

    let parts = line.split(' ')
    if parts.len != 2: 
      stderr.writeLine "Warning: Skipping invalid line format: ", line
      continue

    let initialSprings = parts[0]
    let initialGroupsStr = parts[1]
    
    var currentGroups: seq[int]
    try:
      currentGroups = initialGroupsStr.split(',').map(parseInt)
    except ValueError:
      stderr.writeLine "Warning: Skipping line with invalid group numbers: ", line
      continue

    var currentSprings: string
    
    if unfoldFactor > 1:
        var unfoldedSpringsSeq: seq[string]
        var unfoldedGroupsSeq: seq[int]
        for i in 0 ..< unfoldFactor:
            unfoldedSpringsSeq.add(initialSprings)
            unfoldedGroupsSeq.add(currentGroups) # Add the whole seq[int]
        
        currentSprings = unfoldedSpringsSeq.join("?")
        
        # Flatten the sequence of sequences for groups
        var flatGroups: seq[int]
        for groupSeq in unfoldedGroupsSeq:
            flatGroups.add(groupSeq)
        currentGroups = flatGroups
    else:
        currentSprings = initialSprings
        # Groups are already correct

    var memo: Cache = initTable[(int, int), Count]()
    let arrangements = countArrangements(currentSprings, currentGroups, 0, 0, memo)
    totalArrangements += arrangements
    # echo line, " -> ", arrangements # Uncomment for debugging individual lines

  return totalArrangements

proc main() =
  let filename = "input.txt"
  if not fileExists(filename):
    stderr.writeLine "Error: Input file '", filename, "' not found."
    quit(1)

  let input = readFile(filename)
  let lines = input.strip().splitLines()

  # Part 1
  let result1 = solve(lines, 1)
  echo "Part 1: ", result1

  # Part 2
  let result2 = solve(lines, 5)
  echo "Part 2: ", result2

when isMainModule:
  main()
