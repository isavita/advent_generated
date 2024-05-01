import strutils, sequtils, sets

const favoriteNumber = 1362

type Point = object
  x, y: int

proc isWall(x, y: int): bool =
  var num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
  var bits = 0
  while num > 0:
    if num mod 2 == 1:
      inc bits
    num = num div 2
  return bits mod 2 != 0

proc bfsMaxSteps(start: Point, maxSteps: int): int =
  var visited = initHashSet[Point]()
  var queue: seq[Point] = @[start]
  visited.incl start
  var steps = 0

  while queue.len > 0 and steps < maxSteps:
    let size = queue.len
    for i in 0..<size:
      let point = queue[i]
      for delta in @[Point(x: 1, y: 0), Point(x: -1, y: 0), Point(x: 0, y: 1), Point(x: 0, y: -1)]:
        let next = Point(x: point.x + delta.x, y: point.y + delta.y)
        if next.x >= 0 and next.y >= 0 and not isWall(next.x, next.y) and not visited.contains(next):
          visited.incl next
          queue.add next
    queue = queue[size..<queue.len]
    inc steps

  return visited.len

let start = Point(x: 1, y: 1)
let reachableLocations = bfsMaxSteps(start, 50)
echo reachableLocations