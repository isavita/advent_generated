
import strutils, sequtils, os

type Position = tuple[i, j: int]

proc findPosition(mat: seq[string], ch: char): Position =
  for i, row in mat.pairs:
    for j, c in row.pairs:
      if c == ch:
        return (i, j)
  return (-1, -1)

proc ok(mat: seq[string], st: Position, seq: string): bool =
  var curr = st
  for ch in seq:
    if mat[curr.i][curr.j] == ' ':
      return false
    case ch
    of '^':
      curr.i -= 1
    of 'v':
      curr.i += 1
    of '<':
      curr.j -= 1
    of '>':
      curr.j += 1
    else:
      discard
    if curr.i < 0 or curr.i >= mat.len or curr.j < 0 or curr.j >= mat[0].len:
      return false
  return true

proc generateMoves(position: Position, objective: char, pad: seq[string]): string =
  let objPos = findPosition(pad, objective)
  var ret = ""
  if position.j > objPos.j:
    ret &= repeat("<", position.j - objPos.j)
  if position.i > objPos.i:
    ret &= repeat("^", position.i - objPos.i)
  if position.i < objPos.i:
    ret &= repeat("v", objPos.i - position.i)
  if position.j < objPos.j:
    ret &= repeat(">", objPos.j - position.j)

  if not ok(pad, position, ret):
    ret = ""
    if position.j < objPos.j:
      ret &= repeat(">", objPos.j - position.j)
    if position.i > objPos.i:
      ret &= repeat("^", position.i - objPos.i)
    if position.i < objPos.i:
      ret &= repeat("v", objPos.i - position.i)
    if position.j > objPos.j:
      ret &= repeat("<", position.j - objPos.j)
  return ret

proc solve(code: string, robots: int, keyPad: seq[string], robotPad: seq[string], maxRobots: int): int =
  if robots <= 0:
    return code.len
  var ret = 0
  var posi = 3
  var posj = 2
  if robots != maxRobots:
    posi = 0
  for ch in code:
    var moves: string
    if robots == maxRobots:
      moves = generateMoves((posi, posj), ch, keyPad)
      let pos = findPosition(keyPad, ch)
      posi = pos.i
      posj = pos.j
    else:
      moves = generateMoves((posi, posj), ch, robotPad)
      let pos = findPosition(robotPad, ch)
      posi = pos.i
      posj = pos.j
    ret += solve(moves & "A", robots - 1, keyPad, robotPad, maxRobots)
  return ret

let maxRobots = 3
let keyPad = @[
  "789",
  "456",
  "123",
  " 0A"
]
let robotPad = @[
  " ^A",
  "<v>"
]

var ret = 0
for line in lines("input.txt"):
  let code = line.strip()
  if code.len == 0:
    continue
  var numericPart = 0
  for ch in code:
    if ch >= '0' and ch <= '9':
      numericPart = numericPart * 10 + (ord(ch) - ord('0'))
  ret += solve(code, maxRobots, keyPad, robotPad, maxRobots) * numericPart

echo ret
