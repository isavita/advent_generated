import os
import strutils

var file = open("input.txt")
var lines = file.readAll().split("\n\n")

var sum = 0

for group in lines:
  var people = group.split("\n")
  var groupYes = 0
  var questions = newSeq[int](26)

  for person in people:
    for question in person:
      questions[ord(question) - ord('a')] += 1

  for question in questions:
    if question == len(people):
      groupYes += 1

  sum += groupYes

echo sum