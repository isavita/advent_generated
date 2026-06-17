
import "io" for File

var text = File.read("input.txt")
var r = 0
var c = 0

for (line in text.split("\n")) {
  var points = line.codePoints

  var i = line.indexOf("row ")
  if (i != null) {
    var j = i + 4
    while (j < points.count && points[j] >= 48 && points[j] <= 57) {
      r = r * 10 + points[j] - 48
      j = j + 1
    }
  }

  i = line.indexOf("column ")
  if (i != null) {
    var j = i + 7
    while (j < points.count && points[j] >= 48 && points[j] <= 57) {
      c = c * 10 + points[j] - 48
      j = j + 1
    }
  }
}

var n = r + c - 1
var pos = n * (n - 1) / 2 + c
var exp = pos - 1
var base = 252533
var result = 1

while (exp > 0) {
  if (exp % 2 == 1) result = (result * base) % 33554393
  base = (base * base) % 33554393
  exp = (exp / 2).floor
}

System.print((20151125 * result) % 33554393)
