
import "io" for File

var cubes = []
var cubeSet = {}

var file = File.read("input.txt")
var lines = file.split("\n")
for (line in lines) {
  if (line.trim() == "") continue
  var parts = line.split(",")
  var x = Num.fromString(parts[0].trim())
  var y = Num.fromString(parts[1].trim())
  var z = Num.fromString(parts[2].trim())
  cubes.add([x, y, z])
  cubeSet["%(x),%(y),%(z)"] = true
}

if (cubes.count == 0) {
  System.print(0)
  return
}

var minX = cubes[0][0]
var maxX = cubes[0][0]
var minY = cubes[0][1]
var maxY = cubes[0][1]
var minZ = cubes[0][2]
var maxZ = cubes[0][2]

for (cube in cubes) {
  var x = cube[0]
  var y = cube[1]
  var z = cube[2]
  if (x < minX) minX = x
  if (x > maxX) maxX = x
  if (y < minY) minY = y
  if (y > maxY) maxY = y
  if (z < minZ) minZ = z
  if (z > maxZ) maxZ = z
}

var start = [minX-1, minY-1, minZ-1]
var visited = {}
var queue = [start]
visited["%(start[0]),%(start[1]),%(start[2])"] = true

var exposed = 0
var head = 0
while (head < queue.count) {
  var current = queue[head]
  head = head + 1
  var x = current[0]
  var y = current[1]
  var z = current[2]
  var neighbors = [
    [x+1, y, z],
    [x-1, y, z],
    [x, y+1, z],
    [x, y-1, z],
    [x, y, z+1],
    [x, y, z-1]
  ]
  for (neighbor in neighbors) {
    var nx = neighbor[0]
    var ny = neighbor[1]
    var nz = neighbor[2]
    if (nx < minX-1 || nx > maxX+1 || ny < minY-1 || ny > maxY+1 || nz < minZ-1 || nz > maxZ+1) {
      continue
    }
    var nkey = "%(nx),%(ny),%(nz)"
    if (!visited.containsKey(nkey)) {
      if (!cubeSet.containsKey(nkey)) {
        visited[nkey] = true
        queue.add(neighbor)
      } else {
        exposed = exposed + 1
      }
    }
  }
}

System.print(exposed)
