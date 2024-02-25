import os, strutils

type
  BrightnessGrid = array[0..999, array[0..999, int]]
  Instruction = object
    action: string
    startX, startY, endX, endY: int

# Parses a line of input into an Instruction
proc parseInstruction(line: string): Instruction =
  var
    action: string
    parts: seq[string]

  if line.startsWith("toggle"):
    action = "toggle"
    parts = line.replace("toggle ", "").split(" through ")
  elif line.startsWith("turn on"):
    action = "turn on"
    parts = line.replace("turn on ", "").split(" through ")
  elif line.startsWith("turn off"):
    action = "turn off"
    parts = line.replace("turn off ", "").split(" through ")

  let startCoords = parts[0].split(',')
  let endCoords = parts[1].split(',')

  return Instruction(
    action: action,
    startX: parseInt(startCoords[0]),
    startY: parseInt(startCoords[1]),
    endX: parseInt(endCoords[0]),
    endY: parseInt(endCoords[1])
  )

# Applies an instruction to the brightness grid
proc applyInstruction(grid: var BrightnessGrid, instruction: Instruction) =
  for x in instruction.startX..instruction.endX:
    for y in instruction.startY..instruction.endY:
      case instruction.action
      of "turn on": grid[x][y] += 1
      of "turn off": grid[x][y] = max(0, grid[x][y] - 1)
      of "toggle": grid[x][y] += 2
      else: discard

# Main procedure to process the instructions and calculate the total brightness
proc main(filename: string) =
  var
    grid: BrightnessGrid
    totalBrightness = 0

  for line in lines(filename):
    let instruction = parseInstruction(line)
    applyInstruction(grid, instruction)

  for x in 0..999:
    for y in 0..999:
      totalBrightness += grid[x][y]

  echo "Total brightness: ", totalBrightness

main("input.txt")
