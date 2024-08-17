import "io" for File

var abs = Fn.new { |x|
  if (x < 0) {
    return -x
  }
  return x
}

var max = Fn.new { |a, b|
  if (a > b) {
    return a
  }
  return b
}

var distance = Fn.new { |x, y, z|
  return (abs.call(x) + abs.call(y) + abs.call(z)) / 2
}

var file = File.read("input.txt")
var directions = file.split(",")

var x = 0
var y = 0
var z = 0
var maxDistance = 0

for (dir in directions) {
  if (dir == "n") {
    y = y + 1
    z = z - 1
  } else if (dir == "ne") {
    x = x + 1
    z = z - 1
  } else if (dir == "se") {
    x = x + 1
    y = y - 1
  } else if (dir == "s") {
    y = y - 1
    z = z + 1
  } else if (dir == "sw") {
    x = x - 1
    z = z + 1
  } else if (dir == "nw") {
    x = x - 1
    y = y + 1
  }

  var curDistance = distance.call(x, y, z)
  maxDistance = max.call(maxDistance, curDistance)
}

System.print(maxDistance)