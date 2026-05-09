
import "io" for File

class HeightProfile {
  static fromSchematic(schematic, isLock) {
    var profile = List.filled(5, 0)
    for (col in 0...5) {
      var height = 0
      if (isLock) {
        for (row in 1..5) {
          if (schematic[row][col] == "#") {
            height = height + 1
          } else {
            break
          }
        }
      } else {
        for (row in 5..1) {
          if (schematic[row][col] == "#") {
            height = height + 1
          } else {
            break
          }
        }
      }
      profile[col] = height
    }
    return profile
  }
}

var locks = []
var keys = []
var current = []

var lines = File.read("input.txt").split("\n")
for (line in lines) {
  // Remove possible carriage return and trim
  var trimmed = line.trim()
  if (trimmed == "") {
    if (current.count == 7) {
      var first = current[0]
      var last = current[6]
      if (first == "#####" && last == ".....") {
        locks.add(HeightProfile.fromSchematic(current, true))
      } else if (first == "....." && last == "#####") {
        keys.add(HeightProfile.fromSchematic(current, false))
      }
      current = []
    }
  } else {
    if (current.count < 7) {
      current.add(trimmed)
    }
  }
}
// Last schematic
if (current.count == 7) {
  var first = current[0]
  var last = current[6]
  if (first == "#####" && last == ".....") {
    locks.add(HeightProfile.fromSchematic(current, true))
  } else if (first == "....." && last == "#####") {
    keys.add(HeightProfile.fromSchematic(current, false))
  }
}

var count = 0
for (lock in locks) {
  for (key in keys) {
    var fit = true
    for (col in 0...5) {
      if (lock[col] + key[col] > 5) {
        fit = false
        break
      }
    }
    if (fit) count = count + 1
  }
}
System.print(count)
