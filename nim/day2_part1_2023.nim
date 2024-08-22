import strutils, sequtils, strformat

type
  CubeColor = enum
    red, green, blue
  CubeCount = array[CubeColor, int]

proc parseCubes(cubeStr: string): CubeCount =
  var cubeCount: CubeCount
  let cubes = cubeStr.split(", ")
  for cube in cubes:
    let parts = cube.split(" ")
    let count = parseInt(parts[0])
    let color = parts[1]
    case color
    of "red": cubeCount[red] += count
    of "green": cubeCount[green] += count
    of "blue": cubeCount[blue] += count
    else: discard
  return cubeCount

proc isPossibleGame(cubeCount: CubeCount, maxCubes: CubeCount): bool =
  for color in CubeColor:
    if cubeCount[color] > maxCubes[color]:
      return false
  return true

proc main() =
  let maxCubes: CubeCount = [red: 12, green: 13, blue: 14]
  var possibleGameSum = 0

  for line in lines("input.txt"):
    let parts = line.split(": ")
    let gameId = parseInt(parts[0].split(" ")[1])
    let cubeSets = parts[1].split("; ")

    var possibleGame = true
    for cubeSet in cubeSets:
      let cubeCount = parseCubes(cubeSet)
      if not isPossibleGame(cubeCount, maxCubes):
        possibleGame = false
        break

    if possibleGame:
      possibleGameSum += gameId

  echo possibleGameSum

main()