
import std/os
import std/strutils
import std/sequtils
import std/tables
import std/hashes
import std/sets

type Point = tuple[x, y: int]
type Garden = HashSet[Point]

proc parseData(lines: seq[string]): (Garden, Point) =
  var garden = initHashSet[Point]()
  var start: Point = (-1, -1)
  for y, line in lines.pairs:
    for x, c in line:
      let p: Point = (x, y)
      if c != '#':
        garden.incl(p)
      if c == 'S':
        start = p
  if start.x == -1:
    raise newException(ValueError, "No start 'S' found!")
  return (garden, start)

proc pointMod(p: Point, modVal: int): Point =
  # Add large multiple of modVal to ensure positivity before modulo
  result.x = (p.x + 10 * modVal) mod modVal
  result.y = (p.y + 10 * modVal) mod modVal

proc quadraticFunction(n, a, b, c: int): int =
  # Formula derived from fitting a quadratic ax^2 + bx + c through points (0,a), (1,b), (2,c)
  # and evaluating at n. Using integer arithmetic carefully.
  # Simplified form: a + n*(b-a) + n*(n-1)/2 * (c - 2*b + a)
  let term1 = a
  let term2 = n * (b - a)
  let term3 = (n * (n - 1) div 2) * (c - 2 * b + a)
  return term1 + term2 + term3


proc calculateNumEnds(garden: Garden, start: Point, numIterations: int, maxSize: int): int =
  var queue = initHashSet[Point]()
  queue.incl(start)
  var done = newSeq[int]()
  let targetRem = (maxSize - 1) div 2
  let directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]

  for i in 0 ..< 3 * maxSize: # Simulate enough steps to establish the quadratic pattern
    if (i mod maxSize) == targetRem:
        done.add(queue.len)
    if done.len == 3:
        break

    var newQueue = initHashSet[Point]()
    for p in queue:
        for dir in directions:
            let nextP: Point = (p.x + dir[0], p.y + dir[1])
            let modP = pointMod(nextP, maxSize)
            if garden.contains(modP):
                newQueue.incl(nextP)
    queue = newQueue

  if done.len < 3:
      raise newException(ValueError, "Did not collect enough data points for quadratic fit")

  let n = numIterations div maxSize
  result = quadraticFunction(n, done[0], done[1], done[2])


proc main() =
  let content = readFile("input.txt")
  let lines = content.strip.splitLines()
  if lines.len == 0:
      echo "Input file is empty or invalid."
      return

  let (garden, start) = parseData(lines)
  let maxSize = lines.len # Assuming square grid
  let result = calculateNumEnds(garden, start, 26501365, maxSize)
  echo result

main()
