import strutils, sequtils, math

let input = readFile("input.txt").strip.parseInt div 11
var houses = newSeq[int](input + 1)

for elf in 1..input:
  for house in countup(elf, input, elf):
    if house <= elf * 50:
      houses[house] += elf
    else:
      break

for i, presents in houses:
  if presents >= input:
    echo i
    break