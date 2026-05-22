
import "io" for File

var getNumbers = Fn.new { |line|
  var tokens = []
  var word = ""
  for (i in 0...line.count) {
    var c = line[i]
    if (c == " " || c == "\t" || c == "\r" || c == "\n") {
      if (word != "") {
        tokens.add(word)
        word = ""
      }
    } else {
      word = word + c
    }
  }
  if (word != "") tokens.add(word)
  if (tokens.count > 1) {
    var res = []
    for (i in 1...tokens.count) {
      res.add(Num.fromString(tokens[i]))
    }
    return res
  }
  return []
}

var lines = File.read("input.txt").split("\n")
if (lines.count >= 2) {
  var times = getNumbers.call(lines[0])
  var distances = getNumbers.call(lines[1])
  var totalWays = 1
  for (i in 0...times.count) {
    var time = times[i]
    var record = distances[i]
    var disc = (time * time - 4 * record).sqrt
    var low = ((time - disc) / 2).floor + 1
    var high = ((time + disc) / 2).ceil - 1
    totalWays = totalWays * (high - low + 1)
  }
  System.print(totalWays)
}
