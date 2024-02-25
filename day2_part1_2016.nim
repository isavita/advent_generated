
import strutils

var file = open("input.txt")
var instructions: seq[string] = @[]
for line in file.lines:
  instructions.add(line)

var keypad = @[[1, 2, 3], [4, 5, 6], [7, 8, 9]]
var x = 1
var y = 1
var code = ""

for instruction in instructions:
  for move in instruction:
    case move
    of 'U':
      if x > 0:
        x -= 1
    of 'D':
      if x < 2:
        x += 1
    of 'L':
      if y > 0:
        y -= 1
    of 'R':
      if y < 2:
        y += 1
    else:
      discard
  code.add($keypad[x][y])

echo code
