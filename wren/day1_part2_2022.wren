import "io" for File

var content = File.read("input.txt")
var elfBlocks = content.split("\n\n")
var elves = []
for (block in elfBlocks) {
  if (block.trim() == "") continue
  var lines = block.split("\n")
  var sum = 0
  for (line in lines) {
    if (line != "") {
      sum = sum + Num.fromString(line)
    }
  }
  elves.add(sum)
}
elves.sort {|a, b| a > b}
System.print("Part 1: %(elves[0])")
System.print("Part 2: %(elves[0] + elves[1] + elves[2])")