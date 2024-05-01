import std/[sequtils, strutils, streams]

proc react(polymer: string): string =
  var stack: seq[char]
  for c in polymer:
    if stack.len > 0 and abs(ord(c) - ord(stack[^1])) == 32:
      stack.delete(stack.high)
    else:
      stack.add(c)
  return stack.join("")

let input = newFileStream("input.txt")
var polymer = ""
if input != nil:
  polymer = input.readLine()
  input.close()
else:
  echo "Unable to open input file"
  quit 1

echo "Units remaining: ", react(polymer).len