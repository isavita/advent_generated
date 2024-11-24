
import std/[md5, strutils, deques]

type
  Point = object
    x, y: int
    path: string

proc readPasscode(filename: string): string =
  result = readFile(filename).strip()

proc md5Hash(input: string): string =
  $getMD5(input)

proc getOpenDoors(passcode, path: string): seq[string] =
  let hash = md5Hash(passcode & path)
  for i, c in ['U', 'D', 'L', 'R']:
    if hash[i] in 'b'..'f':
      result.add($c)

proc findLongestPathLength(passcode: string): int =
  var queue = initDeque[Point]()
  queue.addLast(Point(x: 0, y: 0, path: ""))
  
  while queue.len > 0:
    let point = queue.popFirst()
    
    if point.x == 3 and point.y == 3:
      result = max(result, point.path.len)
      continue
    
    for dir in getOpenDoors(passcode, point.path):
      var nextPoint = point
      nextPoint.path.add(dir)
      
      case dir
      of "U": dec nextPoint.y
      of "D": inc nextPoint.y
      of "L": dec nextPoint.x
      of "R": inc nextPoint.x
      
      if nextPoint.x in 0..3 and nextPoint.y in 0..3:
        queue.addLast(nextPoint)

proc main() =
  let passcode = readPasscode("input.txt")
  echo findLongestPathLength(passcode)

main()
