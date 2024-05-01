import strutils, sequtils

proc solve(): int =
  let input = readFile("input.txt").strip.split(", ")
  var x, y = 0
  let dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)] # North, East, South, West
  var dirIndex = 0

  for instr in input:
    let turn = instr[0]
    let steps = parseInt(instr[1..^1])
    if turn == 'R':
      dirIndex = (dirIndex + 1) mod 4
    else:
      dirIndex = (dirIndex - 1 + 4) mod 4
    let (dx, dy) = dirs[dirIndex]
    x += dx * steps
    y += dy * steps

  abs(x) + abs(y)

echo solve()