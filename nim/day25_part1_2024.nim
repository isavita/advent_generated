
import std/[os, strutils, sequtils]

const
  InputFileName = "input.txt"
  NumColumns = 5
  SchematicHeight = 7
  AvailableSpace = 5 # Total height available for lock pin + key shape

type
  HeightProfile = array[NumColumns, int]

# Calculates the height profile for a given schematic.
# For locks (isLock=true), counts '#' downwards from the second row.
# For keys (isLock=false), counts '#' upwards from the second-to-last row.
proc calculateHeightProfile(schematic: seq[string], isLock: bool): HeightProfile =
  assert schematic.len == SchematicHeight, "Invalid schematic height"
  assert schematic[0].len == NumColumns, "Invalid schematic width"

  for col in 0 ..< NumColumns:
    var height = 0
    if isLock:
      # Lock pins extend downwards from row index 1
      for row in 1 ..< SchematicHeight - 1: # Check rows 1 through 5
        if schematic[row][col] == '#':
          height.inc()
        else:
          break # Pin ends when '.' is encountered
    else:
      # Key shapes extend upwards from row index SchematicHeight - 2 (which is 5)
      for row in countdown(SchematicHeight - 2, 1): # Check rows 5 down through 1
        if schematic[row][col] == '#':
          height.inc()
        else:
          break # Key shape ends when '.' is encountered
    result[col] = height

# Checks if a lock and key profile fit together.
proc fits(lockProfile: HeightProfile, keyProfile: HeightProfile): bool =
  for col in 0 ..< NumColumns:
    if lockProfile[col] + keyProfile[col] > AvailableSpace:
      return false # Overlap in this column
  return true # No overlap in any column

# Main procedure to read input, process schematics, and count fitting pairs.
proc main() =
  var locks: seq[HeightProfile] = @[]
  var keys: seq[HeightProfile] = @[]
  var currentSchematic: seq[string] = @[]

  # Read the input file line by line, grouping schematics
  for line in lines(InputFileName):
    let trimmedLine = line.strip() # Remove leading/trailing whitespace

    if trimmedLine.len == 0:
      # Empty line potentially signifies the end of a schematic
      if currentSchematic.len > 0:
        if currentSchematic.len == SchematicHeight:
          let firstRow = currentSchematic[0]
          let lastRow = currentSchematic[^1] # ^1 is Nim's index for the last element

          if firstRow == "#####" and lastRow == ".....":
            # Found a lock
            locks.add(calculateHeightProfile(currentSchematic, true))
          elif firstRow == "....." and lastRow == "#####":
            # Found a key
            keys.add(calculateHeightProfile(currentSchematic, false))
          # else: Ignore schematics that don't match lock/key patterns
        else:
          # Handle potential malformed schematics if needed, currently ignored
          stderr.writeLine("Warning: Ignoring schematic group with incorrect line count: ", currentSchematic.len)

        currentSchematic = @[] # Reset for the next schematic
    else:
      # Add non-empty line to the current schematic buffer
      if currentSchematic.len < SchematicHeight:
          currentSchematic.add(line) # Use original line to preserve structure
      # else: If more than SchematicHeight lines, it's likely an error or separator, ignore extra lines


  # Process the last schematic if the file doesn't end with a blank line
  if currentSchematic.len == SchematicHeight:
    let firstRow = currentSchematic[0]
    let lastRow = currentSchematic[^1]
    if firstRow == "#####" and lastRow == ".....":
      locks.add(calculateHeightProfile(currentSchematic, true))
    elif firstRow == "....." and lastRow == "#####":
      keys.add(calculateHeightProfile(currentSchematic, false))

  # Count the number of fitting lock/key pairs
  var fittingPairsCount = 0
  for lockProfile in locks:
    for keyProfile in keys:
      if lockProfile.fits(keyProfile):
        fittingPairsCount.inc()

  # Print the final result to standard output
  echo fittingPairsCount

# Ensure main runs only when the script is executed directly
when isMainModule:
  main()
