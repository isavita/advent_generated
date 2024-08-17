import "io" for File

var parseRange = Fn.new { |r|
  var parts = r.split("-")
  return [Num.fromString(parts[0]), Num.fromString(parts[1])]
}

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")
  var count = 0

  for (line in lines) {
    var ranges = line.split(",")
    if (ranges.count != 2) continue

    var range1 = parseRange.call(ranges[0])
    var start1 = range1[0]
    var end1 = range1[1]

    var range2 = parseRange.call(ranges[1])
    var start2 = range2[0]
    var end2 = range2[1]

    if ((start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1)) {
      count = count + 1
    }
  }

  System.print(count)
}

main.call()