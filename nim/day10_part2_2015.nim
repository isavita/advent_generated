
import strutils

var input: string

input = readFile("input.txt")

proc lookAndSay(s: string): string =
  var result: string = ""
  var count = 1
  for i in 0..<s.len:
    if i + 1 < s.len and s[i] == s[i + 1]:
      count += 1
    else:
      result.add($count)
      result.add(s[i])
      count = 1
  result

for _ in 0..39:
  input = lookAndSay(input)

echo input.len

for _ in 0..9:
  input = lookAndSay(input)

echo input.len
