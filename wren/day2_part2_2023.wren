import "io" for File

var input = File.read("input.txt")
var lines = input.split("\n")
var totalPower = 0

for (line in lines) {
  var parts = line.split(": ")
  var rounds = parts[1].split("; ")
  var maxRed = 0
  var maxGreen = 0
  var maxBlue = 0

  for (round in rounds) {
    var cubes = round.split(", ")
    var red = 0
    var green = 0
    var blue = 0

    for (cube in cubes) {
      var countColor = cube.split(" ")
      var count = Num.fromString(countColor[0])
      var color = countColor[1]

      if (color == "red") {
        red = count
      } else if (color == "green") {
        green = count
      } else if (color == "blue") {
        blue = count
      }

      if (red > maxRed) maxRed = red
      if (green > maxGreen) maxGreen = green
      if (blue > maxBlue) maxBlue = blue
    }
  }

  var power = maxRed * maxGreen * maxBlue
  totalPower = totalPower + power
}

System.print(totalPower)