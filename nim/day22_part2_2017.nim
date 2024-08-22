import tables, sequtils, strutils

type
  Direction = enum Up, Right, Down, Left
  State = enum Clean, Weakened, Infected, Flagged
  Grid = Table[(int, int), State]

proc turnLeft(d: Direction): Direction =
  case d
  of Up: Left
  of Right: Up
  of Down: Right
  of Left: Down

proc turnRight(d: Direction): Direction =
  case d
  of Up: Right
  of Right: Down
  of Down: Left
  of Left: Up

proc reverse(d: Direction): Direction =
  case d
  of Up: Down
  of Right: Left
  of Down: Up
  of Left: Right

proc move(pos: (int, int), d: Direction): (int, int) =
  case d
  of Up: (pos[0], pos[1] - 1)
  of Right: (pos[0] + 1, pos[1])
  of Down: (pos[0], pos[1] + 1)
  of Left: (pos[0] - 1, pos[1])

proc simulate(grid: var Grid, bursts: int): int =
  var pos = (0, 0)
  var dir = Up
  var infections = 0

  for _ in 1..bursts:
    let state = grid.getOrDefault(pos, Clean)
    case state
    of Clean: dir = turnLeft(dir)
    of Weakened: discard
    of Infected: dir = turnRight(dir)
    of Flagged: dir = reverse(dir)

    case state
    of Clean: grid[pos] = Weakened
    of Weakened:
      grid[pos] = Infected
      infections.inc
    of Infected: grid[pos] = Flagged
    of Flagged: grid[pos] = Clean

    pos = move(pos, dir)

  infections

when isMainModule:
  let input = readFile("input.txt").splitLines().mapIt(it.toSeq)
  var grid: Grid
  let mid = input.len div 2
  for y, row in input:
    for x, cell in row:
      if cell == '#':
        grid[(x - mid, y - mid)] = Infected

  let result = simulate(grid, 10000000)
  echo result