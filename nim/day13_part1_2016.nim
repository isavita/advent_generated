import strutils, sequtils, sets

type Point = object
  x, y: int

proc isWall(favoriteNumber, x, y: int): bool =
  var num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
  var bits = 0
  while num > 0:
    if num mod 2 == 1:
      inc bits
    num = num div 2
  return bits mod 2 != 0

proc bfs(start, target: Point, favoriteNumber: int): int =
  var visited: HashSet[Point] = initHashSet[Point]()
  var queue: seq[Point] = @[start]
  var steps = 0

  while queue.len > 0:
    let size = queue.len
    for i in 0..<size:
      let point = queue[i]
      if point == target:
        return steps
      for delta in @[Point(x: 1, y: 0), Point(x: -1, y: 0), Point(x: 0, y: 1), Point(x: 0, y: -1)]:
        let next = Point(x: point.x + delta.x, y: point.y + delta.y)
        if next.x >= 0 and next.y >= 0 and not isWall(favoriteNumber, next.x, next.y) and not visited.contains(next):
          visited.incl next
          queue.add next
    queue.delete(0..<size)
    inc steps

  return -1

when isMainModule:
  let input = readFile("input.txt").strip
  let favoriteNumber = input.parseInt
  let start = Point(x: 1, y: 1)
  let target = Point(x: 31, y: 39)
  let steps = bfs(start, target, favoriteNumber)
  echo steps