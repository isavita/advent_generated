import strutils, sequtils, tables, algorithm

type
  OrbitMap = Table[string, string]

proc parseOrbits(filename: string): OrbitMap =
  var orbits: OrbitMap
  for line in lines(filename):
    let parts = line.split(")")
    orbits[parts[1]] = parts[0]
  orbits

proc countOrbits(orbits: OrbitMap, obj: string, count: int): int =
  if obj notin orbits:
    return count
  return countOrbits(orbits, orbits[obj], count + 1)

proc totalOrbits(orbits: OrbitMap): int =
  var total = 0
  for obj in orbits.keys:
    total += countOrbits(orbits, obj, 0)
  total

proc pathToCOM(orbits: OrbitMap, obj: string): seq[string] =
  var path: seq[string]
  var current = obj
  while current in orbits:
    current = orbits[current]
    path.add(current)
  path

proc minTransfers(orbits: OrbitMap, start, target: string): int =
  let startPath = pathToCOM(orbits, start)
  let targetPath = pathToCOM(orbits, target)
  var commonAncestor: string
  for obj in startPath:
    if obj in targetPath:
      commonAncestor = obj
      break
  let startTransfers = startPath.find(commonAncestor)
  let targetTransfers = targetPath.find(commonAncestor)
  startTransfers + targetTransfers

let orbits = parseOrbits("input.txt")
echo "Total orbits: ", totalOrbits(orbits)
echo "Minimum transfers: ", minTransfers(orbits, "YOU", "SAN")