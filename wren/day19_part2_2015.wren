
import "io" for File

var lines = File.read("input.txt").replace("\r", "").split("\n")
while (lines.count > 0 && lines[-1] == "") {
  lines.removeAt(-1)
}
var molecule = lines[-1]

var total = 0
var rn = 0
var ar = 0
var y = 0
var i = 0
var len = molecule.count

while (i < len) {
  var code = molecule[i].codePoints[0]
  if (code >= 65 && code <= 90) {
    total = total + 1
    var elem = molecule[i]
    if (i + 1 < len) {
      var nextCode = molecule[i+1].codePoints[0]
      if (nextCode >= 97 && nextCode <= 122) {
        elem = elem + molecule[i+1]
        i = i + 2
      } else {
        i = i + 1
      }
    } else {
      i = i + 1
    }
    if (elem == "Rn") {
      rn = rn + 1
    } else if (elem == "Ar") {
      ar = ar + 1
    } else if (elem == "Y") {
      y = y + 1
    }
  } else {
    i = i + 1
  }
}

System.print(total - rn - ar - 2 * y - 1)
