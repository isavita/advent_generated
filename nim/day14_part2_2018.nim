import strutils, sequtils

let file = readFile("input.txt")
var scoreboard = @[3, 7]
var elf1, elf2: int
elf1 = 0
elf2 = 1

var inputSequence: seq[int]
for c in file:
  inputSequence.add parseInt($c)

proc checkSequence(scoreboard, sequence: seq[int]): bool =
  if scoreboard.len < sequence.len:
    return false
  let start = scoreboard.len - sequence.len
  for i, v in sequence:
    if scoreboard[start + i] != v:
      return false
  return true

while true:
  let newScore = scoreboard[elf1] + scoreboard[elf2]
  if newScore >= 10:
    scoreboard.add(newScore div 10)
    if checkSequence(scoreboard, inputSequence):
      break
  scoreboard.add(newScore mod 10)
  if checkSequence(scoreboard, inputSequence):
    break

  elf1 = (elf1 + scoreboard[elf1] + 1) mod scoreboard.len
  elf2 = (elf2 + scoreboard[elf2] + 1) mod scoreboard.len

echo scoreboard.len - inputSequence.len