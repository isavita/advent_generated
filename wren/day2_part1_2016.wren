
import "io" for File

var keypad = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9]
]

var instructions = File.read("input.txt").split("\n")

var x = 1
var y = 1
var code = ""

for (instruction in instructions) {
  for (move in instruction) {
    if (move == "U" && x > 0) x = x - 1
    if (move == "D" && x < 2) x = x + 1
    if (move == "L" && y > 0) y = y - 1
    if (move == "R" && y < 2) y = y + 1
  }
  code = code + keypad[x][y].toString
}

System.print(code)
