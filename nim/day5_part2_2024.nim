
import std/[strutils, sequtils, tables, sets, deques, algorithm]

#------------------------------------------------------------------------------
# Helper Procedures
#------------------------------------------------------------------------------

proc parseInput(input: string): (HashSet[(int, int)], seq[seq[int]]) =
  ## Parses the input string into ordering rules and update sequences.
  var rules = initHashSet[(int, int)]()
  var updates = newSeq[seq[int]]()
  var readingRules = true

  for line in input.strip.splitLines:
    if line.len == 0:
      readingRules = false
      continue

    if readingRules:
      # Example: "47|53"
      let parts = line.split('|')
      if parts.len == 2:
        try:
          let u = parts[0].parseInt
          let v = parts[1].parseInt
          rules.incl((u, v))
        except ValueError:
          stderr.writeLine("Warning: Skipping invalid rule line: ", line)
      else:
        stderr.writeLine("Warning: Skipping invalid rule line format: ", line)
    else:
      # Example: "75,47,61,53,29"
      try:
        updates.add(line.split(',').map(parseInt))
      except ValueError:
        stderr.writeLine("Warning: Skipping invalid update line: ", line)

  return (rules, updates)

proc isOrderCorrect(update: seq[int], rules: HashSet[(int, int)], pagesInUpdate: HashSet[int]): bool =
  ## Checks if the given update sequence respects the ordering rules.
  ## Assumes pagesInUpdate contains all unique elements from 'update'.
  if update.len <= 1:
    return true # Single page or empty updates are trivially correct

  # Iterate through all pairs (pageI, pageJ) where pageI appears before pageJ
  for i in 0 ..< update.len:
    for j in i + 1 ..< update.len:
      let pageI = update[i]
      let pageJ = update[j]

      # Check if a rule exists that mandates J must come before I.
      # This check only matters if both pages are actually in this specific update,
      # which is implicitly true since we are iterating over the update itself.
      # The check `pageI in pagesInUpdate and pageJ in pagesInUpdate` is redundant here
      # but necessary if checking rules against arbitrary pairs.
      if rules.contains((pageJ, pageI)):
        # Found a violation: rule says J before I, but update has I before J.
        return false

  # No violations found
  return true

proc topologicalSort(update: seq[int], rules: HashSet[(int, int)], pagesInUpdate: HashSet[int]): seq[int] =
  ## Performs a topological sort on the pages in the update based on the rules.
  ## Returns the correctly ordered sequence of pages.
  ## Assumes pagesInUpdate contains all unique elements from 'update'.
  if update.len <= 1:
    return update # Already sorted

  var
    adj = initTable[int, seq[int]]()       # Adjacency list (successors)
    inDegree = initTable[int, int]()    # In-degree count for each page
    queue = initDeque[int]()            # Queue for nodes with in-degree 0
    sortedUpdate = newSeq[int]()        # Resulting sorted sequence

  # Initialize in-degree for all pages present in this update
  for page in pagesInUpdate:
    inDegree[page] = 0

  # Build the graph (adjacency list) and calculate initial in-degrees
  # considering only pages present in the current update.
  for (u, v) in rules:
    if u in pagesInUpdate and v in pagesInUpdate:
      # Add edge u -> v
      if not adj.hasKey(u): adj[u] = @[]
      adj[u].add(v)
      # Increment in-degree of the destination node v
      inDegree[v] = inDegree.getOrDefault(v, 0) + 1 # Should already exist, but safe

  # Initialize the queue with all nodes having an in-degree of 0.
  # Sort these initial nodes numerically for deterministic output, although
  # any valid topological order works. Sorting helps consistency.
  var initialNodes: seq[int]
  for page in pagesInUpdate:
    if inDegree.getOrDefault(page, 0) == 0: # Use getOrDefault in case a page had no rules
        initialNodes.add(page)
  initialNodes.sort(cmp[int])
  for node in initialNodes:
      queue.addLast(node)


  # Process the queue (Kahn's algorithm)
  while queue.len > 0:
    let u = queue.popFirst()
    sortedUpdate.add(u)

    # For each neighbor v of u
    if adj.hasKey(u):
      # Sort neighbors for deterministic output if multiple choices exist at this step
      var neighbors = adj[u]
      neighbors.sort(cmp[int]) # Optional optimization for determinism

      for v in neighbors: # Use the potentially sorted neighbors list
        inDegree[v] -= 1
        # If in-degree becomes 0, add v to the queue
        if inDegree[v] == 0:
          queue.addLast(v)

  # Verification: Check if all pages were included in the sort
  if sortedUpdate.len != pagesInUpdate.len:
     # This implies a cycle, which shouldn't occur based on problem description
     # but is a good safeguard.
     raise newException(ValueError, "Cycle detected in rules for update or error in sort.")

  return sortedUpdate

proc getMiddlePage(update: seq[int]): int =
  ## Returns the middle page number of an update sequence.
  if update.len == 0:
    raise newException(ValueError, "Cannot get middle page of empty update.")
  # Integer division `div` automatically handles floor for middle index
  let midIndex = update.len div 2
  return update[midIndex]

#------------------------------------------------------------------------------
# Solve Procedure
#------------------------------------------------------------------------------

proc solve(input: string): (int, int) =
  ## Solves both parts of the puzzle.
  ## Returns (part1_sum, part2_sum).
  let (rules, updates) = parseInput(input)

  var part1Sum = 0 # Sum of middle pages for initially correct updates
  var part2Sum = 0 # Sum of middle pages for re-ordered incorrect updates

  for originalUpdate in updates:
    if originalUpdate.len == 0: continue # Skip empty updates

    # Create a HashSet for quick lookups of pages within this specific update
    let pagesInUpdate = toHashSet(originalUpdate)

    if isOrderCorrect(originalUpdate, rules, pagesInUpdate):
      # Part 1: Update is already correct
      part1Sum += getMiddlePage(originalUpdate)
    else:
      # Part 2: Update is incorrect, need to re-order
      let correctlyOrderedUpdate = topologicalSort(originalUpdate, rules, pagesInUpdate)
      part2Sum += getMiddlePage(correctlyOrderedUpdate)

  return (part1Sum, part2Sum)

#------------------------------------------------------------------------------
# Main Entry Point
#------------------------------------------------------------------------------

proc main() =
  let inputFile = "input.txt"
  try:
    let inputContent = readFile(inputFile)
    let (resultPart1, resultPart2) = solve(inputContent)
    echo "Part 1: ", resultPart1
    echo "Part 2: ", resultPart2
  except IOError:
    stderr.writeLine("Error: Could not read input file '", inputFile, "'")
  except ValueError as e:
    stderr.writeLine("Error processing input: ", e.msg)
  except Exception as e:
    stderr.writeLine("An unexpected error occurred: ", e.msg)

# Execute the main procedure
when isMainModule:
  main()
