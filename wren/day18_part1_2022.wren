
import "io" for File

var lines = File.read("input.txt").split("\n")
var cubes = []
var cubeSet = {}

for (line in lines) {
  if (line.isEmpty) continue
  var parts = line.split(",")
  var x = Num.fromString(parts[0])
  var y = Num.fromString(parts[1])
  var z = Num.fromString(parts[2])
  cubes.add([x, y, z])
  cubeSet["%(x),%(y),%(z)"] = true
}

var directions = [[-1,0,0], [1,0,0], [0,-1,0], [0,1,0], [0,0,-1], [0,0,1]]
var surfaceArea = 0

for (cube in cubes) {
  var x = cube[0]
  var y = cube[1]
  var z = cube[2]
  for (d in directions) {
    var nx = x + d[0]
    var ny = y + d[1]
    var nz = z + d[2]
    var key = "%(nx),%(ny),%(nz)"
    if (cubeSet[key] == null) {
      surfaceArea = surfaceArea + 1
    }
  }
}

System.print(surfaceArea)
