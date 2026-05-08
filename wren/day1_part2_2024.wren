
import "io" for File

var content = File.read("input.txt")
var leftList = []
var rightCounts = {}

var lines = content.split("\n")
for (line in lines) {
  if (line.trim().isEmpty) continue
  var words = []
  var current = ""
  for (ch in line) {
    if (ch == " " || ch == "\t") {
      if (current != "") {
        words.add(current)
        current = ""
      }
    } else {
      current = current + ch
    }
  }
  if (current != "") words.add(current)
  if (words.count >= 2) {
    var left = Num.fromString(words[0])
    var right = Num.fromString(words[words.count-1])
    leftList.add(left)
    rightCounts[right] = (rightCounts[right] || 0) + 1
  }
}

var similarityScore = 0
for (num in leftList) {
  var count = rightCounts[num] || 0
  similarityScore = similarityScore + num * count
}

System.print("Similarity score: %(similarityScore)")
