import "io" for File

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")
  var totalPaper = 0

  for (line in lines) {
    var dims = line.split("x")
    var l = Num.fromString(dims[0])
    var w = Num.fromString(dims[1])
    var h = Num.fromString(dims[2])

    var sides = [l*w, w*h, h*l]
    sides.sort()

    var paper = 2*l*w + 2*w*h + 2*h*l
    var slack = sides[0]

    totalPaper = totalPaper + paper + slack
  }

  System.print(totalPaper)
}

main.call()