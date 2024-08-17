import "io" for File

var count = 0

var file = File.read("input.txt")
var lines = file.split("\n")

for (line in lines) {
  var parts = line.split(" | ")
  var output = parts[1].split(" ")

  for (code in output) {
    var len = code.count
    if (len == 2 || len == 4 || len == 3 || len == 7) {
      count = count + 1
    }
  }
}

System.print(count)