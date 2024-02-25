import os, strutils

type
  LightGrid = array[0..999, array[0..999, bool]]
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

# Applies an instruction to the grid
proc applyInstruction(grid: var LightGrid, instruction: Instruction) =
  for x in instruction.startX..instruction.endX:
    for y in instruction.startY..instruction.endY:
      case instruction.action
      of "turn on": grid[x][y] = true
      of "turn off": grid[x][y] = false
      of "toggle": grid[x][y] = not grid[x][y]
      else: discard

# Main procedure to process the instructions and count the lights that are on
proc main(filename: string) =
  var
    grid: LightGrid
    lightsOn = 0

  for line in lines(filename):
    let instruction = parseInstruction(line)
    applyInstruction(grid, instruction)

  for x in 0..999:
    for y in 0..999:
      if grid[x][y]:
        inc(lightsOn)

  echo "Lights that are on: ", lightsOn

main("input.txt")
