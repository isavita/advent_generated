import "io" for File

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")

  var codeChars = 0
  var memoryChars = 0
  var encodedChars = 0

  for (line in lines) {
    line = line.trim()
    codeChars = codeChars + line.count

    var i = 0
    while (i < line.count) {
      if (line[i] == "\\") {
        if (line[i + 1] == "x") {
          i = i + 3
        } else {
          i = i + 1
        }
      } else {
        memoryChars = memoryChars + 1
      }
      i = i + 1
    }

    var encodedLine = "\"" + line.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    encodedChars = encodedChars + encodedLine.count
  }

  System.print(codeChars - memoryChars)
  System.print(encodedChars - codeChars)
}

main.call()