import "io" for File

var splitWhitespace = Fn.new { |s|
  var parts = []
  var curr = ""
  for (c in s) {
    if (c == " " || c == "\t" || c == "\n" || c == "\r") {
      if (curr != "") {
        parts.add(curr)
        curr = ""
      }
    } else {
      curr = curr + c
    }
  }
  if (curr != "") parts.add(curr)
  return parts
}

var readInput = Fn.new {
  var content = File.read("input.txt")
  var lines = content.split("\n")
  var input = []
  for (line in lines) {
    line = line.trim()
    if (line == "") continue
    var tokens = splitWhitespace.call(line)
    var row = []
    for (token in tokens) {
      row.add(Num.fromString(token))
    }
    input.add(row)
  }
  return input
}

var part1 = Fn.new { |input|
  var total = 0
  for (row in input) {
    var minVal = row[0]
    var maxVal = row[0]
    for (val in row) {
      if (val < minVal) minVal = val
      if (val > maxVal) maxVal = val
    }
    total = total + (maxVal - minVal)
  }
  return total
}

var part2 = Fn.new { |input|
  var total = 0
  for (row in input) {
    for (i in 0...row.count) {
      for (j in i+1...row.count) {
        var a = row[i]
        var b = row[j]
        if (a % b == 0) {
          total = total + a / b
        } else if (b % a == 0) {
          total = total + b / a
        }
      }
    }
  }
  return total
}

var input = readInput.call()
System.print("Part 1: %(part1.call(input))")
System.print("Part 2: %(part2.call(input))")