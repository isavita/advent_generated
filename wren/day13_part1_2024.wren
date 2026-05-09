import "io" for File

var parseBlock = Fn.new { |block|
  var lines = block.split("\n")
  var aLine = lines[0]
  var bLine = lines[1]
  var pLine = lines[2]
  var aParts = aLine.split(", ")
  var aX = Num.fromString(aParts[0].split("+")[1])
  var aY = Num.fromString(aParts[1].split("+")[1])
  var bParts = bLine.split(", ")
  var bX = Num.fromString(bParts[0].split("+")[1])
  var bY = Num.fromString(bParts[1].split("+")[1])
  var pParts = pLine.split(", ")
  var pX = Num.fromString(pParts[0].split("=")[1])
  var pY = Num.fromString(pParts[1].split("=")[1])
  return {"a": [aX, aY], "b": [bX, bY], "prize": [pX, pY]}
}

var content = File.read("input.txt")
var blocks = content.split("\n\n")
var machines = []
for (block in blocks) {
  if (block.trim() != "") machines.add(parseBlock.call(block))
}
var totalTokens = 0
for (machine in machines) {
  var minTokens = 1000000
  var a = machine["a"]
  var b = machine["b"]
  var prize = machine["prize"]
  for (aCount in 0..100) {
    for (bCount in 0..100) {
      var x = a[0]*aCount + b[0]*bCount
      var y = a[1]*aCount + b[1]*bCount
      if (x == prize[0] && y == prize[1]) {
        var tokens = aCount*3 + bCount
        if (tokens < minTokens) minTokens = tokens
      }
    }
  }
  if (minTokens != 1000000) totalTokens = totalTokens + minTokens
}
System.print(totalTokens)