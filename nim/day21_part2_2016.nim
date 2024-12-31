
import strutils, sequtils, algorithm, os

type Scrambler = object
  pw: seq[char]

proc newScrambler(pw: string): Scrambler =
  Scrambler(pw: pw.toSeq)

proc `$`(s: Scrambler): string =
  s.pw.join

proc swapPositions(s: var Scrambler, x, y: int) =
  (s.pw[x], s.pw[y]) = (s.pw[y], s.pw[x])

proc swapLetters(s: var Scrambler, x, y: char) =
  s.swapPositions(s.pw.find(x), s.pw.find(y))

proc rotate(s: var Scrambler, steps: int) =
  let length = s.pw.len
  var steps = steps mod length
  if steps < 0:
    steps += length
  s.pw = s.pw[length - steps ..< length] & s.pw[0 ..< length - steps]

proc rotateLetter(s: var Scrambler, x: char) =
  var index = s.pw.find(x)
  if index >= 4:
    inc index
  s.rotate(index + 1)

proc derotateLetter(s: var Scrambler, x: char) =
  let index = s.pw.find(x)
  var rot: int
  if index mod 2 == 1:
    rot = -(index + 1) div 2
  elif index != 0:
    rot = (6 - index) div 2
  else:
    rot = -1
  s.rotate(rot)

proc reverse(s: var Scrambler, x, y: int) =
  var x = x
  var y = y
  while x < y:
    (s.pw[x], s.pw[y]) = (s.pw[y], s.pw[x])
    inc x
    dec y

proc move(s: var Scrambler, x, y: int) =
  let ch = s.pw[x]
  if x < y:
    for i in x..<y:
      s.pw[i] = s.pw[i+1]
  else:
    for i in countdown(x, y+1):
      s.pw[i] = s.pw[i-1]
  s.pw[y] = ch

proc scramble(s: var Scrambler, instructions: seq[string], direction: int): Scrambler =
  var instructions = instructions
  if direction < 0:
    instructions.reverse()
  for instruction in instructions:
    let line = instruction.split()
    case line[0]
    of "swap":
      let x = line[2]
      let y = line[^1]
      if line[1] == "position":
        s.swapPositions(parseInt(x), parseInt(y))
      else:
        s.swapLetters(x[0], y[0])
    of "rotate":
      if line[1] == "based":
        if direction > 0:
          s.rotateLetter(line[^1][0])
        else:
          s.derotateLetter(line[^1][0])
      else:
        var x = parseInt(line[2])
        if line[1] == "left":
          x = -x
        if direction < 0:
          x = -x
        s.rotate(x)
    of "reverse":
      s.reverse(parseInt(line[2]), parseInt(line[^1]))
    of "move":
      var x = parseInt(line[2])
      var y = parseInt(line[^1])
      if direction < 0:
        (x, y) = (y, x)
      s.move(x, y)
  return s

proc unscramble(s: var Scrambler, instructions: seq[string]): Scrambler =
  s.scramble(instructions, -1)

proc main() =
  let instructions = readFile("input.txt").splitLines()
  var scrambler = newScrambler("fbgdceah")
  let result = scrambler.unscramble(instructions)
  echo result

main()
