import strutils, sequtils, tables

type Direction = enum
  Down, Up, Left, Right

type Position = object
  x, y: int

proc readDiagram(filename: string): seq[seq[char]] =
  let file = readFile(filename)
  let lines = file.splitLines()
  var maxWidth = 0
  for line in lines:
    if line.len > maxWidth:
      maxWidth = line.len
  result = newSeq[seq[char]](lines.len)
  for i, line in lines:
    result[i] = newSeq[char](maxWidth)
    for j, c in line:
      result[i][j] = c

proc findStart(diagram: seq[seq[char]]): Position =
  for x, c in diagram[0]:
    if c == '|':
      return Position(x: x, y: 0)
  raise newException(IOError, "No starting point found")

proc followPath(diagram: seq[seq[char]]): string =
  var pos = findStart(diagram)
  var direction = Down
  var seenLetters = ""
  while true:
    case direction
    of Down:
      inc pos.y
    of Up:
      dec pos.y
    of Left:
      dec pos.x
    of Right:
      inc pos.x
    let current = diagram[pos.y][pos.x]
    if current in {'|', '-'}:
      continue
    elif current == '+':
      if direction == Down or direction == Up:
        if pos.x > 0 and diagram[pos.y][pos.x - 1] != ' ':
          direction = Left
        else:
          direction = Right
      else:
        if pos.y > 0 and diagram[pos.y - 1][pos.x] != ' ':
          direction = Up
        else:
          direction = Down
    elif current.isAlphaNumeric():
      seenLetters.add current
    else:
      break
  return seenLetters

let diagram = readDiagram("input.txt")
echo followPath(diagram)