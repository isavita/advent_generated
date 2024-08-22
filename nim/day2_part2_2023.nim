import strutils, sequtils, strformat

type
  CubeColor = enum
    red, green, blue
  CubeSet = object
    red, green, blue: int

proc parseCubeSet(s: string): CubeSet =
  var result: CubeSet
  for color in s.split(", "):
    let parts = color.split(" ")
    let count = parseInt(parts[0])
    let colorName = parts[1]
    case colorName
    of "red": result.red = count
    of "green": result.green = count
    of "blue": result.blue = count
    else: discard
  result

proc isPossibleGame(cubeSets: seq[CubeSet], maxRed, maxGreen, maxBlue: int): bool =
  for cubeSet in cubeSets:
    if cubeSet.red > maxRed or cubeSet.green > maxGreen or cubeSet.blue > maxBlue:
      return false
  true

proc findMinimumCubes(cubeSets: seq[CubeSet]): CubeSet =
  var minCubes: CubeSet
  for cubeSet in cubeSets:
    minCubes.red = max(minCubes.red, cubeSet.red)
    minCubes.green = max(minCubes.green, cubeSet.green)
    minCubes.blue = max(minCubes.blue, cubeSet.blue)
  minCubes

proc calculatePower(cubeSet: CubeSet): int =
  cubeSet.red * cubeSet.green * cubeSet.blue

when isMainModule:
  let file = readFile("input.txt")
  let lines = file.splitLines()

  var possibleGameSum = 0
  var minimumPowerSum = 0

  for line in lines:
    let parts = line.split(": ")
    let gameId = parseInt(parts[0].split(" ")[1])
    let cubeSets = parts[1].split("; ").map(parseCubeSet)

    if isPossibleGame(cubeSets, 12, 13, 14):
      possibleGameSum += gameId

    let minCubes = findMinimumCubes(cubeSets)
    minimumPowerSum += calculatePower(minCubes)

  echo "Part 1: ", possibleGameSum
  echo "Part 2: ", minimumPowerSum