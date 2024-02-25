
import strutils

var file = open("input.txt")
var lines = file.readAll.splitLines

var totalScore = 0

for line in lines:
  var opponent = line[0]
  var yourMove = line[2]

  var score = 0
  if yourMove == 'X':
    score = 1
  elif yourMove == 'Y':
    score = 2
  elif yourMove == 'Z':
    score = 3

  if (opponent == 'A' and yourMove == 'Y') or (opponent == 'B' and yourMove == 'Z') or (opponent == 'C' and yourMove == 'X'):
    score += 6
  elif (opponent == 'A' and yourMove == 'X') or (opponent == 'B' and yourMove == 'Y') or (opponent == 'C' and yourMove == 'Z'):
    score += 3

  totalScore += score

echo totalScore
