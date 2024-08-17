import "io" for File

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")

  var horizontalPosition = 0
  var depth = 0
  var aim = 0

  for (line in lines) {
    var parts = line.split(" ")
    var direction = parts[0]
    var units = Num.fromString(parts[1])

    if (direction == "forward") {
      horizontalPosition = horizontalPosition + units
      depth = depth + aim * units
    } else if (direction == "down") {
      aim = aim + units
    } else if (direction == "up") {
      aim = aim - units
    }
  }

  var product = horizontalPosition * depth
  System.print(product)
}

main.call()