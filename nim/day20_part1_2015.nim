import strutils, sequtils

let input = readFile("input.txt").strip.parseInt div 10
var houses = newSeq[int](input + 1)

for elf in 1..input:
  for house in countup(elf, input, elf):
    houses[house] += elf

for i, presents in houses:
  if presents >= input:
    echo i
    break