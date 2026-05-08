
import "io" for File

class Fuel {
  static calculate(mass) {
    var total = 0
    var current = mass
    while (true) {
      var fuel = (current/3).floor - 2
      if (fuel <= 0) break
      total = total + fuel
      current = fuel
    }
    return total
  }
}

var input = File.read("input.txt")
var lines = input.split("\n")
var total = 0
for (line in lines) {
  line = line.trim()
  if (line == "") continue
  var mass = Num.fromString(line)
  if (mass == null) continue
  total = total + Fuel.calculate(mass)
}
System.print(total)
