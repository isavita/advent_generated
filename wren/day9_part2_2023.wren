
import "io" for File

class Solver {
  static parseLine(line) {
    var nums = []
    var curr = ""
    for (c in line) {
      if (c == " " || c == "\t" || c == "\n" || c == "\r") {
        if (curr != "") {
          nums.add(Num.fromString(curr))
          curr = ""
        }
      } else {
        curr = curr + c
      }
    }
    if (curr != "") nums.add(Num.fromString(curr))
    return nums
  }

  static allZeros(seq) {
    for (num in seq) {
      if (num != 0) return false
    }
    return true
  }

  static extrapolation(seq) {
    var result = []
    for (i in 1...seq.count) {
      result.add(seq[i] - seq[i-1])
    }
    return result
  }

  static extrapolations(history) {
    var result = [history]
    for (i in 1...history.count) {
      var prev = result[i-1]
      if (allZeros(prev)) return result
      result.add(extrapolation(prev))
    }
    return result
  }

  static pastPrediction(history) {
    var rows = extrapolations(history)
    var pred = 0
    for (i in (rows.count-1)..0) {
      pred = rows[i][0] - pred
    }
    return pred
  }
}

var input = File.read("input.txt")
var lines = input.split("\n")
var total = 0
for (line in lines) {
  if (line.trim() == "") continue
  var nums = Solver.parseLine(line)
  total = total + Solver.pastPrediction(nums)
}
System.print(total)
