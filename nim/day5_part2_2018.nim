import std/sequtils
import std/strutils
import std/algorithm

proc reactPolymer(polymer: string): string =
  var stack: seq[char]
  for c in polymer:
    if stack.len > 0 and abs(ord(c) - ord(stack[^1])) == 32:
      stack.delete(stack.high)
    else:
      stack.add(c)
  result = stack.join("")

proc shortestPolymerLength(polymer: string): int =
  var shortest = polymer.len
  for c in 'a'..'z':
    let newPolymer = polymer.replace($c, "").replace($(chr(ord(c) - 32)), "")
    let reactedPolymer = reactPolymer(newPolymer)
    shortest = min(shortest, reactedPolymer.len)
  result = shortest

when isMainModule:
  let input = readFile("input.txt")
  let reactedPolymer = reactPolymer(input)
  echo "Part 1: ", reactedPolymer.len
  echo "Part 2: ", shortestPolymerLength(input)