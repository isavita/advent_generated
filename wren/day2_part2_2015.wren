import "io" for File

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")
  var totalRibbon = 0

  for (line in lines) {
    var dimensions = line.split("x")
    if (dimensions.count != 3) {
      System.print("Invalid input format")
      return
    }

    var l = Num.fromString(dimensions[0])
    var w = Num.fromString(dimensions[1])
    var h = Num.fromString(dimensions[2])

    var bow = l * w * h
    var sides = [l, w, h]
    sides.sort()
    var wrap = 2 * sides[0] + 2 * sides[1]

    totalRibbon = totalRibbon + bow + wrap
  }

  System.print(totalRibbon)
}

main.call()