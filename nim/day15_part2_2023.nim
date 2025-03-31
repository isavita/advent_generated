
import strutils
import sequtils
import os

# --- HASH Algorithm ---
proc calculateHash(s: string): int =
  result = 0
  for c in s:
    result = (result + ord(c)) * 17 mod 256

# --- Lens Data Structure ---
type
  Lens = object
    label: string
    focalLength: int

# --- Helper to find lens index in a box ---
proc findLensIndex(box: seq[Lens], label: string): int =
  for i, lens in box.pairs:
    if lens.label == label:
      return i
  return -1

# --- Main Logic ---
proc solve(input: string): (int, int) =
  # Split input into steps, handling potential trailing newline first
  let steps = input.strip().split(',')

  # --- Part 1: Calculate sum of HASHes ---
  var part1Sum = 0
  for step in steps:
    part1Sum += calculateHash(step)

  # --- Part 2: Simulate HASHMAP ---
  var boxes: array[0..255, seq[Lens]] # Array of 256 sequences (boxes)
  for i in 0..255:
      boxes[i] = newSeq[Lens]() # Initialize each box as an empty sequence

  for step in steps:
    var label: string
    var operation: char
    var focalLengthStr: string = ""

    # Parse the step
    if step.contains('-'):
      let opIndex = step.find('-')
      label = step[0 ..< opIndex]
      operation = '-'
    elif step.contains('='):
      let opIndex = step.find('=')
      label = step[0 ..< opIndex]
      operation = '='
      focalLengthStr = step[opIndex + 1 .. ^1]
    else:
      # Should not happen based on problem description, but good to handle
      stderr.writeLine("Warning: Invalid step format encountered: " & step)
      continue

    # Determine the box index
    let boxIndex = calculateHash(label)

    # Perform the operation
    let box = addr boxes[boxIndex] # Get a mutable reference to the sequence
    let existingLensIndex = findLensIndex(box[], label) # Pass the dereferenced sequence

    if operation == '-':
      if existingLensIndex != -1:
        box[].delete(existingLensIndex) # Remove the lens
    elif operation == '=':
      let focalLength = parseInt(focalLengthStr)
      if existingLensIndex != -1:
        # Replace existing lens's focal length
        box[][existingLensIndex].focalLength = focalLength
      else:
        # Add new lens to the end of the box
        box[].add(Lens(label: label, focalLength: focalLength))

  # Calculate total focusing power
  var totalFocusingPower = 0
  for boxIndex in 0..255:
    for slotIndex, lens in boxes[boxIndex].pairs:
      # Slot numbers are 1-based, pairs gives 0-based index
      let power = (boxIndex + 1) * (slotIndex + 1) * lens.focalLength
      totalFocusingPower += power

  return (part1Sum, totalFocusingPower)

# --- Entry Point ---
proc main() =
  # Read input from file
  let inputFilename = "input.txt"
  if not fileExists(inputFilename):
    stderr.writeLine("Error: Input file not found: " & inputFilename)
    quit(1)

  let fileContent = readFile(inputFilename)

  # Solve the problem
  let (part1Result, part2Result) = solve(fileContent)

  # Print the results to standard output
  echo "Part 1: ", part1Result
  echo "Part 2: ", part2Result

when isMainModule:
  main()
