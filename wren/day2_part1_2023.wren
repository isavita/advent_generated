
import "io" for File

var maxCubes = {"red": 12, "green": 13, "blue": 14}

var parseCubes = Fn.new { |s|
  var counts = {"red": 0, "green": 0, "blue": 0}
  var cubes = s.split(", ")
  for (cube in cubes) {
    var parts = cube.split(" ")
    var count = Num.fromString(parts[0])
    var color = parts[1]
    counts[color] = counts[color] + count
  }
  return counts
}

var isPossible = Fn.new { |counts|
  for (color in maxCubes.keys) {
    if (counts[color] > maxCubes[color]) return false
  }
  return true
}

var main = Fn.new {
  var sum = 0
  var content = File.read("input.txt")
  var lines = content.split("\n")
  for (line in lines) {
    if (line.trim() == "") continue
    var parts = line.split(": ")
    var gameId = Num.fromString(parts[0].split(" ")[1])
    var sets = parts[1].split("; ")
    var possible = true
    for (set in sets) {
      var counts = parseCubes.call(set)
      if (!isPossible.call(counts)) {
        possible = false
        break
      }
    }
    if (possible) sum = sum + gameId
  }
  System.print(sum)
}

main.call()
