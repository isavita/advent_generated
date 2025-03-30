
import sets, strutils, sequtils, collections/deques

const GridSize = 71
const InputFileName = "input.txt"
const MaxLines = 1024

type Coord = tuple[x, y: int]
type State = tuple[c: Coord, steps: int]

proc solve() =
  var corrupted = initHashSet[Coord]()
  block read_input:
    var i = 0
    for line in lines(InputFileName):
      if i >= MaxLines: break
      let parts = line.strip().split(',')
      if parts.len == 2:
        try:
          let x = parseInt(parts[0].strip())
          let y = parseInt(parts[1].strip())
          corrupted.incl((x, y))
        except ValueError:
          discard # Ignore invalid lines
      inc i

  var q = initDeque[State]()
  var visited = initHashSet[Coord]()
  let startCoord: Coord = (0, 0)
  let targetCoord: Coord = (GridSize - 1, GridSize - 1)

  q.addLast((startCoord, 0))
  visited.incl(startCoord)

  let directions: array[4, Coord] = [(0, 1), (0, -1), (1, 0), (-1, 0)]

  while q.len > 0:
    let (currentCoord, steps) = q.popFirst()

    if currentCoord == targetCoord:
      echo steps
      return

    for d in directions:
      let nextCoord: Coord = (currentCoord.x + d.x, currentCoord.y + d.y)

      if nextCoord.x >= 0 and nextCoord.x < GridSize and
         nextCoord.y >= 0 and nextCoord.y < GridSize:
          if not corrupted.contains(nextCoord) and not visited.contains(nextCoord):
            visited.incl(nextCoord)
            q.addLast((nextCoord, steps + 1))

when isMainModule:
  solve()
