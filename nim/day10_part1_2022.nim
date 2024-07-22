
import strutils, sequtils

# Function to read instructions from a file
proc readInstructions(filename: string): seq[string] =
  let fileContent = readFile(filename)
  result = fileContent.splitLines()

# Function to execute the instructions and calculate signal strengths
proc calculateSignalStrengths(instructions: seq[string]): int =
  var X = 1
  var cycle = 0
  var signalStrengths = 0
  let targetCycles = {20, 60, 100, 140, 180, 220}
  
  for instruction in instructions:
    if instruction.startsWith("noop"):
      cycle += 1
      if cycle in targetCycles:
        signalStrengths += cycle * X
    elif instruction.startsWith("addx"):
      cycle += 1
      if cycle in targetCycles:
        signalStrengths += cycle * X
      cycle += 1
      if cycle in targetCycles:
        signalStrengths += cycle * X
      let value = instruction.split(' ')[1].parseInt()
      X += value

  return signalStrengths

# Main program
proc main() =
  let instructions = readInstructions("input.txt")
  let totalSignalStrength = calculateSignalStrengths(instructions)
  echo totalSignalStrength

# Run the main program
main()
