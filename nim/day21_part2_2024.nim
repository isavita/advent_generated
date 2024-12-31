
import strutils, sequtils, tables, os

type
  Position = tuple[i, j: int]
  CacheKey = tuple[pos: Position, ch: char, padType: string]
  SolveKey = tuple[code: string, robots, maxRobots: int]

var
  positionCache: Table[string, Position]
  okCache: Table[string, bool]
  moveCache: Table[CacheKey, string]
  solveCache: Table[SolveKey, int]

proc findPosition(mat: seq[string], ch: char): Position =
  let key = $ch & mat.join()
  if key in positionCache:
    return positionCache[key]
  for i, row in mat:
    for j, c in row:
      if c == ch:
        let pos = (i, j)
        positionCache[key] = pos
        return pos
  return (-1, -1)

proc ok(mat: seq[string], st: Position, seq: string): bool =
  let key = $st.i & "," & $st.j & "," & seq & "," & mat.join()
  if key in okCache:
    return okCache[key]
  var curr = st
  for ch in seq:
    if mat[curr.i][curr.j] == ' ':
      okCache[key] = false
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
      okCache[key] = false
      return false
  okCache[key] = true
  return true

proc generateMoves(position: Position, objective: char, pad: seq[string]): string =
  let key = (position, objective, pad.join())
  if key in moveCache:
    return moveCache[key]
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
  var result = ret
  if not ok(pad, position, result):
    ret = ""
    if position.j < objPos.j:
      ret &= repeat(">", objPos.j - position.j)
    if position.i > objPos.i:
      ret &= repeat("^", position.i - objPos.i)
    if position.i < objPos.i:
      ret &= repeat("v", objPos.i - position.i)
    if position.j > objPos.j:
      ret &= repeat("<", position.j - objPos.j)
    result = ret
  moveCache[key] = result
  return result

proc solve(code: string, robots: int, keyPad: seq[string], robotPad: seq[string], maxRobots: int): int =
  let key = (code, robots, maxRobots)
  if key in solveCache:
    return solveCache[key]
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
  solveCache[key] = ret
  return ret

let
  maxRobots = 26
  keyPad = @[
    "789",
    "456",
    "123",
    " 0A"
  ]
  robotPad = @[
    " ^A",
    "<v>"
  ]

var ret = 0
let content = readFile("input.txt").strip().splitLines()
for code in content:
  let code = code.strip()
  if code.len == 0:
    continue
  var numericPart = 0
  for ch in code:
    if ch >= '0' and ch <= '9':
      numericPart = numericPart * 10 + (ord(ch) - ord('0'))
  ret += solve(code, maxRobots, keyPad, robotPad, maxRobots) * numericPart
echo ret
