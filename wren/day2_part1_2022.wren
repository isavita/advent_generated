
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")
var totalScore = 0

var shapeScore = { "X": 1, "Y": 2, "Z": 3 }
var outcomeMap ={
  "AX": 3, "AY": 6, "AZ": 0,
  "BX": 0, "BY": 3, "BZ": 6,
  "CX": 6, "CY": 0, "CZ": 3
}

for (line in lines) {
  if (line.isEmpty) continue
  var opponent = line[0]
  var yourMove = line[2]
  var shape = shapeScore[yourMove]
  var outcome = outcomeMap[opponent + yourMove]
  totalScore = totalScore + shape + outcome
}

System.print(totalScore)
