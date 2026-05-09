import "io" for File

var min = Fn.new { |a,b| a < b ? a : b }
var max = Fn.new { |a,b| a > b ? a : b }

var rocks = {}
var maxY = 0

var content = File.read("input.txt")
var lines = content.split("\n")
for (line in lines) {
  if (line.trim() == "") continue
  var points = line.split(" -> ")
  var prev = null
  for (pointStr in points) {
    var parts = pointStr.split(",")
    var x = Num.fromString(parts[0])
    var y = Num.fromString(parts[1])
    var coord = [x, y]
    if (prev == null) {
      prev = coord
      var key = x.toString + "," + y.toString
      rocks[key] = true
      if (y > maxY) maxY = y
    } else {
      var x0 = prev[0]
      var y0 = prev[1]
      var x1 = coord[0]
      var y1 = coord[1]
      if (x0 == x1) {
        var yMin = min.call(y0, y1)
        var yMax = max.call(y0, y1)
        var y = yMin
        while (y <= yMax) {
          var key = x0.toString + "," + y.toString
          rocks[key] = true
          y = y + 1
        }
      } else if (y0 == y1) {
        var xMin = min.call(x0, x1)
        var xMax = max.call(x0, x1)
        var x = xMin
        while (x <= xMax) {
          var key = x.toString + "," + y0.toString
          rocks[key] = true
          x = x + 1
        }
      }
      if (y1 > maxY) maxY = y1
      prev = coord
    }
  }
}

var copyMap = Fn.new { |map|
  var newMap = {}
  for (key in map.keys) {
    newMap[key] = map[key]
  }
  return newMap
}

var obstacles1 = copyMap.call(rocks)
var sandCount1 = 0
var part1Done = false
while (!part1Done) {
  var sandPos = [500, 0]
  while (true) {
    if (sandPos[1] > maxY) {
      part1Done = true
      break
    }
    var down = [sandPos[0], sandPos[1] + 1]
    var keyDown = down[0].toString + "," + down[1].toString
    if (!obstacles1.containsKey(keyDown)) {
      sandPos = down
      continue
    }
    var downLeft = [sandPos[0] - 1, sandPos[1] + 1]
    var keyDownLeft = downLeft[0].toString + "," + downLeft[1].toString
    if (!obstacles1.containsKey(keyDownLeft)) {
      sandPos = downLeft
      continue
    }
    var downRight = [sandPos[0] + 1, sandPos[1] + 1]
    var keyDownRight = downRight[0].toString + "," + downRight[1].toString
    if (!obstacles1.containsKey(keyDownRight)) {
      sandPos = downRight
      continue
    }
    var key = sandPos[0].toString + "," + sandPos[1].toString
    obstacles1[key] = true
    sandCount1 = sandCount1 + 1
    break
  }
  if (part1Done) break
}

var obstacles2 = copyMap.call(rocks)
var floorY = maxY + 2
var sandCount2 = 0
while (true) {
  var sandPos = [500, 0]
  var keySource = "500,0"
  if (obstacles2.containsKey(keySource)) break
  while (true) {
    var down = [sandPos[0], sandPos[1] + 1]
    var downLeft = [sandPos[0] - 1, sandPos[1] + 1]
    var downRight = [sandPos[0] + 1, sandPos[1] + 1]
    if (down[1] >= floorY) {
      var key = sandPos[0].toString + "," + sandPos[1].toString
      obstacles2[key] = true
      sandCount2 = sandCount2 + 1
      break
    }
    var keyDown = down[0].toString + "," + down[1].toString
    if (!obstacles2.containsKey(keyDown)) {
      sandPos = down
      continue
    }
    var keyDownLeft = downLeft[0].toString + "," + downLeft[1].toString
    if (!obstacles2.containsKey(keyDownLeft)) {
      sandPos = downLeft
      continue
    }
    var keyDownRight = downRight[0].toString + "," + downRight[1].toString
    if (!obstacles2.containsKey(keyDownRight)) {
      sandPos = downRight
      continue
    }
    var key = sandPos[0].toString + "," + sandPos[1].toString
    obstacles2[key] = true
    sandCount2 = sandCount2 + 1
    break
  }
}

System.print("Part 1: %(sandCount1)")
System.print("Part 2: %(sandCount2)")