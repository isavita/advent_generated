
import
  std/heapqueue,
  std/os,
  std/strutils,
  std/sequtils

type
  Position = object
    x, y, risk: int

proc `==`(a, b: Position): bool =
  a.x == b.x and a.y == b.y

proc `<`(a, b: Position): bool =
  a.risk < b.risk

proc dijkstra(grid: seq[seq[int]]): int =
  var pq = initHeapQueue[Position]()
  pq.push(Position(x: 0, y: 0, risk: 0))

  let rows = grid.len
  let cols = grid[0].len
  var dist = newSeqWith(rows, newSeqWith(cols, high(int)))
  dist[0][0] = 0

  let directions = @[Position(x: 1, y: 0), Position(x: 0, y: 1), Position(x: -1, y: 0), Position(x: 0, y: -1)]

  while pq.len > 0:
    let curr = pq.pop()
    if curr.x == rows - 1 and curr.y == cols - 1:
      return curr.risk
    for d in directions:
      let nx = curr.x + d.x
      let ny = curr.y + d.y
      if nx >= 0 and ny >= 0 and nx < rows and ny < cols:
        let nextRisk = curr.risk + grid[nx][ny]
        if nextRisk < dist[nx][ny]:
          dist[nx][ny] = nextRisk
          pq.push(Position(x: nx, y: ny, risk: nextRisk))
  return -1


proc main() =
  let file = open("input.txt")
  defer: file.close()
  var grid: seq[seq[int]]
  for line in file.lines:
    grid.add(line.mapIt(it.ord - ord('0')))

  echo dijkstra(grid)

when isMainModule:
  main()
