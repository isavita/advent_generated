import strutils, sequtils, tables

type
  OrbitMap = Table[string, string]

proc parseOrbitMap(filename: string): OrbitMap =
  let file = readFile(filename)
  let lines = file.splitLines()
  var orbitMap: OrbitMap
  for line in lines:
    let parts = line.split(")")
    let center = parts[0]
    let orbiter = parts[1]
    orbitMap[orbiter] = center
  orbitMap

proc countOrbits(orbitMap: OrbitMap, obj: string, count: int = 0): int =
  if obj notin orbitMap:
    return count
  let center = orbitMap[obj]
  return countOrbits(orbitMap, center, count + 1)

proc totalOrbits(orbitMap: OrbitMap): int =
  var total = 0
  for obj in orbitMap.keys:
    total += countOrbits(orbitMap, obj)
  total

let orbitMap = parseOrbitMap("input.txt")
echo totalOrbits(orbitMap)