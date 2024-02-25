
import os
import strutils

var file = open("input.txt")
let input = file.readAll().strip()

var nice = 0

proc passesRule1(line: string): bool =
  for i in 0..<len(line)-1:
    let toMatch = line[i..i+1]
    for j in i+2..<len(line)-1:
      if line[j..j+1] == toMatch:
        return true
  return false

for line in input.splitLines():
  var rule1 = passesRule1(line)
  var rule2 = false
  for i in 0..<len(line)-2:
    if line[i] == line[i+2]:
      rule2 = true
      break
  if rule1 and rule2:
    nice += 1

echo nice
