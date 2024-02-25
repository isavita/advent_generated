
import strutils

proc lookAndSay(s: string): string =
  var result = ""
  var count = 1
  for i in 0..<s.len:
    if i + 1 < s.len and s[i] == s[i + 1]:
      count += 1
    else:
      result.add($count & s[i])
      count = 1
  result

var input = readFile("input.txt").strip
for _ in 0..<40:
  input = lookAndSay(input)

echo input.len
