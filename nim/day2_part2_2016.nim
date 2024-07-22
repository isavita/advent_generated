
import strutils, sequtils, os

# Define the keypad layout
const keypad = [
  "  1  ",
  " 234 ",
  "56789",
  " ABC ",
  "  D  "
]

# Function to get the next position based on the current position and the move
proc move(current: (int, int), direction: char): (int, int) =
  let (x, y) = current
  case direction:
    of 'U': return (x - 1, y)
    of 'D': return (x + 1, y)
    of 'L': return (x, y - 1)
    of 'R': return (x, y + 1)
    else: return current

# Function to check if the position is valid on the keypad
proc isValidPosition(pos: (int, int)): bool =
  let (x, y) = pos
  return x >= 0 and x < keypad.len and y >= 0 and y < keypad[x].len and keypad[x][y] != ' '

# Function to find the bathroom code based on the instructions
proc findBathroomCode(instructions: seq[string]): string =
  var code = ""
  var currentPos = (3, 1)  # Starting at '5' which is at (3, 1)

  for line in instructions:
    for moveDir in line:
      let newPos = move(currentPos, moveDir)
      if isValidPosition(newPos):
        currentPos = newPos
    code.add(keypad[currentPos[0]][currentPos[1]])

  return code

# Main procedure to read input and print the bathroom code
proc main() =
  let inputFile = "input.txt"
  if not fileExists(inputFile):
    echo "Input file not found!"
    return

  let instructions = readFile(inputFile).splitLines()
  let bathroomCode = findBathroomCode(instructions)
  echo "The bathroom code is: ", bathroomCode

# Run the main procedure
main()
