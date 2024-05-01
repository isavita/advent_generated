import strutils, sequtils

proc isPossibleTriangle(a, b, c: int): bool =
  (a + b > c) and (a + c > b) and (b + c > a)

proc countPossibleTriangles(filename: string): int =
  var count = 0
  for line in lines(filename):
    let sides = line.splitWhitespace().mapIt(it.parseInt())
    if isPossibleTriangle(sides[0], sides[1], sides[2]):
      inc count
  return count

echo countPossibleTriangles("input.txt")