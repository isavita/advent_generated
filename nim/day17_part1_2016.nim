
import std/md5
import std/strutils
import std/deques
import std/os

const gridSize = 4

type
  State = tuple[x, y: int, path: string]

proc getHash(inputString: string): string =
  md5.getMD5(inputString)

proc getOpenDoors(hashStr: string): array[4, bool] =
  for i in 0 ..< 4:
    let c = hashStr[i]
    result[i] = c >= 'b' and c <= 'f'

proc isValidMove(x, y: int; direction: char): bool =
  case direction:
  of 'U': y > 0
  of 'D': y < gridSize - 1
  of 'L': x > 0
  of 'R': x < gridSize - 1
  else: false

proc findShortestPath(passcode: string): string =
  const directions = ['U', 'D', 'L', 'R']
  var queue = initDeque[State]()
  queue.addLast((0, 0, ""))

  while queue.len > 0:
    let (x, y, path) = queue.popFirst()

    if x == gridSize - 1 and y == gridSize - 1:
      return path

    let hashInput = passcode & path
    let hashStr = getHash(hashInput)
    let openDoors = getOpenDoors(hashStr)

    for i in 0 ..< 4:
      let direction = directions[i]
      if openDoors[i] and isValidMove(x, y, direction):
        var newX = x
        var newY = y
        case direction:
        of 'U': newY -= 1
        of 'D': newY += 1
        of 'L': newX -= 1
        of 'R': newX += 1
        else: discard

        queue.addLast((newX, newY, path & direction))

  return "" # Should not happen given puzzle constraints

proc main() =
  let passcode = readFile("input.txt").strip()
  let shortestPath = findShortestPath(passcode)
  echo shortestPath

when isMainModule:
  main()
