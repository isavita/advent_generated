
import strutils

type
  State = enum
    group
    garbage
    ignore

proc calculateScore(input: string): int =
  var score = 0
  var state = State.group
  var depth = 0
  var ignoreNext = false

  for char in input:
    case state
    of State.group:
      case char
      of '{':
        inc(depth)
      of '}':
        score += depth
        dec(depth)
      of '<':
        state = State.garbage
      else:
        discard
    of State.garbage:
      case char
      of '!':
        state = State.ignore
      of '>':
        state = State.group
      else:
        discard
    of State.ignore:
      state = State.garbage

  result = score

let input = readFile("input.txt")
echo calculateScore(input)
