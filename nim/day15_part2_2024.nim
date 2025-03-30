
import std/[strutils, sequtils, tables, algorithm]

type Coord = tuple[x, y: int]
type Grid = Table[Coord, char]

proc `+`(a, b: Coord): Coord = (a.x + b.x, a.y + b.y)
proc `*`(s: int, a: Coord): Coord = (a.x * s, a.y * s)

const
  Up: Coord = (x: 0, y: -1)
  Down: Coord = (x: 0, y: 1)
  Left: Coord = (x: -1, y: 0)
  Right: Coord = (x: 1, y: 0)

proc tryToStep(m: var Grid, pos: Coord, dir: Coord): bool =
  var orig = initTable[Coord, char]()
  for k, v in m:
    orig[k] = v

  let targetPos = pos + dir
  let targetChar = m.getOrDefault(targetPos, '#') # Assume outside map is wall '#'

  case m.getOrDefault(pos, '#')
  of '.':
    return true # Can always step from empty space (shouldn't happen in normal flow)
  of 'O', '@':
    if tryToStep(m, targetPos, dir):
      m[targetPos] = m[pos]
      m[pos] = '.'
      return true
  of ']':
    # To move ']', we must successfully move the corresponding '['
    if tryToStep(m, pos + Left, dir):
       # The tryToStep call for '[' will handle moving ']' implicitly
       return true
  of '[':
    let rightPartPos = pos + Right
    if dir == Left:
      let pushTarget = pos + Left
      if tryToStep(m, pushTarget, dir): # Check space left of '['
        m[pushTarget] = '['
        m[pos] = ']' # Old '[' becomes ']'
        m[rightPartPos] = '.' # Old ']' becomes '.'
        return true
    elif dir == Right:
      let pushTarget = pos + 2 * Right # Check space right of ']'
      if tryToStep(m, pushTarget, dir):
        m[pos] = '.' # Old '[' becomes '.'
        m[rightPartPos] = '[' # Old ']' becomes '['
        m[pushTarget] = ']' # Landing spot for ']'
        return true
    else: # Up or Down
      let targetLeft = pos + dir
      let targetRight = rightPartPos + dir
      # Must be able to move both parts
      if tryToStep(m, targetLeft, dir) and tryToStep(m, targetRight, dir):
         # Check succeeded, now perform the actual move after potential recursive changes
         # Re-fetch current state as recursion might have altered it
         if m.getOrDefault(targetLeft) == '.' and m.getOrDefault(targetRight) == '.':
             m[targetLeft] = '['
             m[targetRight] = ']'
             m[pos] = '.'
             m[rightPartPos] = '.'
             return true
  else: discard # Wall '#' or invalid char

  # If move failed, restore original state
  m.clear()
  for k, v in orig:
    m[k] = v
  return false

proc solve(inputStr: string): int =
  let blocks = inputStr.strip().split("\n\n")
  let lines = blocks[0].split('\n')

  var m = initTable[Coord, char]()
  var robot: Coord

  for y, row in lines:
    for x, char in row:
      let pos: Coord = (x, y)
      m[pos] = char
      if char == '@':
        robot = pos

  var steps = newSeq[Coord]()
  for char in blocks[1].replace("\n", ""):
    case char
    of '^': steps.add(Up)
    of '<': steps.add(Left)
    of '>': steps.add(Right)
    of 'v': steps.add(Down)
    else: discard

  for dir in steps:
    if tryToStep(m, robot, dir):
      robot = robot + dir

  var score = 0
  for pos, char in m:
    if char in ['[', 'O']:
      score += pos.x + 100 * pos.y
  return score

proc scaleUp(inputStr: string): string =
  result = inputStr.replace("#", "##")
  result = result.replace(".", "..")
  result = result.replace("O", "[]")
  result = result.replace("@", "@.")

proc main() =
  let inputStr = readFile("input.txt")
  echo solve(inputStr)
  echo solve(scaleUp(inputStr))

main()
