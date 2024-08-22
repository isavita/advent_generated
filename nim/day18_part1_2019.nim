import std/[sequtils, strutils, tables, deques, math, algorithm]

type
  Point = object
    x, y: int
  State = object
    pos: Point
    keys: set[char]
    steps: int

proc bfs(grid: seq[seq[char]], start: Point, totalKeys: int): int =
  let directions = [Point(x: -1, y: 0), Point(x: 1, y: 0), Point(x: 0, y: -1), Point(x: 0, y: 1)]
  var visited = initTable[(Point, set[char]), bool]()
  var queue = initDeque[State]()
  queue.addLast(State(pos: start, keys: {}, steps: 0))

  while queue.len > 0:
    let state = queue.popFirst()
    if state.keys.len == totalKeys:
      return state.steps

    for dir in directions:
      let newPos = Point(x: state.pos.x + dir.x, y: state.pos.y + dir.y)
      if newPos.x < 0 or newPos.x >= grid.len or newPos.y < 0 or newPos.y >= grid[0].len:
        continue

      let cell = grid[newPos.x][newPos.y]
      if cell == '#':
        continue

      var newKeys = state.keys
      if cell in {'a'..'z'}:
        newKeys.incl cell
      elif cell in {'A'..'Z'} and (cell.toLowerAscii notin state.keys):
        continue

      if (newPos, newKeys) notin visited:
        visited[(newPos, newKeys)] = true
        queue.addLast(State(pos: newPos, keys: newKeys, steps: state.steps + 1))

  return -1

when isMainModule:
  let input = readFile("input.txt").strip().splitLines()
  var grid: seq[seq[char]] = @[]
  var start: Point
  var totalKeys = 0

  for x, line in input.pairs:
    var row: seq[char] = @[]
    for y, char in line.pairs:
      row.add char
      if char == '@':
        start = Point(x: x, y: y)
      elif char in {'a'..'z'}:
        totalKeys += 1
    grid.add row

  let result = bfs(grid, start, totalKeys)
  echo result