
import std/[os, strutils, sets, sequtils, parseutils]

const InputFile = "input.txt"

# Function to check if an update respects the ordering rules
proc isValidUpdate(update: seq[int], rules: HashSet[(int, int)]): bool =
  # If the update has 0 or 1 page, it's trivially valid
  if update.len <= 1:
    return true

  # Create a quick lookup for page presence and position
  # Not strictly necessary with the nested loop approach below,
  # but could be useful if logic changed. Keeping the simpler approach for now.

  # Check every pair of pages (pageI, pageJ) where pageI appears before pageJ
  for i in 0 ..< update.len:
    for j in i + 1 ..< update.len:
      let pageI = update[i]
      let pageJ = update[j]

      # Check if the rule requires pageJ to be before pageI
      # If rule (pageJ, pageI) exists, this update is invalid
      if rules.contains((pageJ, pageI)):
        # echo "Violation: Rule ", pageJ, "|", pageI, " broken by update order ", pageI, " then ", pageJ
        return false

  # No violations found
  return true

proc main() =
  # 1. Read Input File
  if not fileExists(InputFile):
    stderr.writeLine "Error: Input file not found: ", InputFile
    quit(1)

  let content = readFile(InputFile)
  let lines = content.strip.splitLines # strip() handles potential trailing newline

  var rules = initHashSet[(int, int)]()
  var updates = newSeq[seq[int]]()
  var parsingRules = true

  # 2. Parse Input
  for line in lines:
    if line.len == 0: # Empty line separates rules from updates
      parsingRules = false
      continue

    if parsingRules:
      # Parse rule X|Y
      let parts = line.split('|')
      if parts.len == 2:
        try:
          let x = parseInt(parts[0].strip())
          let y = parseInt(parts[1].strip())
          rules.incl((x, y))
        except ValueError:
          stderr.writeLine "Warning: Could not parse rule line: ", line
      else:
          stderr.writeLine "Warning: Malformed rule line: ", line

    else:
      # Parse update P1,P2,...
      var currentUpdate = newSeq[int]()
      var success = true
      for pageStr in line.split(','):
        try:
          currentUpdate.add(parseInt(pageStr.strip()))
        except ValueError:
          stderr.writeLine "Warning: Could not parse page number in update: ", pageStr, " on line: ", line
          success = false
          break # Skip this malformed update line
      if success and currentUpdate.len > 0:
         updates.add(currentUpdate)

  # 3. Process Updates
  var totalMiddleSum = 0
  for update in updates:
    if isValidUpdate(update, rules):
      # Calculate middle index (0-based)
      # For len=5, middle is index 2. 5 div 2 = 2
      # For len=3, middle is index 1. 3 div 2 = 1
      let middleIndex = update.len div 2
      totalMiddleSum += update[middleIndex]
      # Optional: Print valid updates and their middle numbers for debugging
      # echo "Valid: ", update, " Middle: ", update[middleIndex]


  # 4. Print Output
  echo totalMiddleSum

# Proper main entry point
when isMainModule:
  main()
