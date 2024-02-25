
import os
import strutils

var sum = 0

let file = open("input.txt")
for line in file.lines:
  var firstDigit = -1
  var lastDigit = -1

  for r in line:
    if r.isDigit:
      let digit = parseInt($r)  # Parse the character r as an integer
      if firstDigit == -1:
        firstDigit = digit
      lastDigit = digit

  if firstDigit != -1 and lastDigit != -1:
    let value = parseInt($firstDigit & $lastDigit)
    sum += value

file.close()

echo sum
