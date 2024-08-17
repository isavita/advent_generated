import "io" for File

var main = Fn.new {
  var input = File.read("input.txt")
  var houses = {}
  var x = 0
  var y = 0
  houses["0,0"] = 1

  for (i in 0..input.count-1) {
    var c = input[i]
    if (c == "^") {
      y = y + 1
    } else if (c == "v") {
      y = y - 1
    } else if (c == ">") {
      x = x + 1
    } else if (c == "<") {
      x = x - 1
    }
    var key = "%(x),%(y)"
    if (houses[key] == null) {
      houses[key] = 1
    } else {
      houses[key] = houses[key] + 1
    }
  }

  var part1 = houses.count

  // Part 2
  x = 0
  y = 0
  var rx = 0
  var ry = 0
  houses = {}
  houses["0,0"] = 2

  for (i in 0..input.count-1) {
    var c = input[i]
    if (i % 2 == 0) {
      if (c == "^") {
        y = y + 1
      } else if (c == "v") {
        y = y - 1
      } else if (c == ">") {
        x = x + 1
      } else if (c == "<") {
        x = x - 1
      }
      var key = "%(x),%(y)"
      if (houses[key] == null) {
        houses[key] = 1
      } else {
        houses[key] = houses[key] + 1
      }
    } else {
      if (c == "^") {
        ry = ry + 1
      } else if (c == "v") {
        ry = ry - 1
      } else if (c == ">") {
        rx = rx + 1
      } else if (c == "<") {
        rx = rx - 1
      }
      var key = "%(rx),%(ry)"
      if (houses[key] == null) {
        houses[key] = 1
      } else {
        houses[key] = houses[key] + 1
      }
    }
  }

  var part2 = houses.count

  System.print("Part 1: %(part1)")
  System.print("Part 2: %(part2)")
}

main.call()