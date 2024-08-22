import std/[sequtils, strutils, tables, algorithm]

let file = "input.txt"
let input = readFile(file).splitLines()

var scores: seq[int] = @[]

for line in input:
  var stack: seq[char] = @[]
  var corrupted = false

  for c in line:
    case c
    of '(':
      stack.add(')')
    of '[':
      stack.add(']')
    of '{':
      stack.add('}')
    of '<':
      stack.add('>')
    of ')', ']', '}', '>':
      if stack.len > 0 and stack[^1] == c:
        stack.delete(stack.high)
      else:
        corrupted = true
        break
    else:
      discard

  if corrupted:
    continue

  var score = 0
  for c in stack.reversed:
    score *= 5
    case c
    of ')':
      score += 1
    of ']':
      score += 2
    of '}':
      score += 3
    of '>':
      score += 4
    else:
      discard

  scores.add(score)

scores.sort()
echo scores[scores.len div 2]