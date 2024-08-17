import "io" for File

var parseRange = Fn.new { |s|
  var split = s.split("-")
  var start = Num.fromString(split[0])
  var end = Num.fromString(split[1])
  return [start, end]
}

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")

  var count = 0
  for (line in lines) {
    var pair = line.split(",")

    var left = parseRange.call(pair[0])
    var right = parseRange.call(pair[1])

    if (left[0] <= right[1] && left[1] >= right[0]) {
      count = count + 1
    }
  }

  System.print(count)
}

main.call()