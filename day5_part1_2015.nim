
import strutils

var niceCount = 0

proc hasThreeVowels(s: string): bool =
  var count = 0
  for c in s:
    if c in {'a', 'e', 'i', 'o', 'u'}:
      count += 1
  return count >= 3

proc hasDoubleLetter(s: string): bool =
  for i in 0..<s.len-1:
    if s[i] == s[i+1]:
      return true
  return false

proc hasDisallowedSubstrings(s: string): bool =
  return "ab" in s or "cd" in s or "pq" in s or "xy" in s

let file = open("input.txt")
for line in file.lines:
  let s = line.strip()
  if hasThreeVowels(s) and hasDoubleLetter(s) and not hasDisallowedSubstrings(s):
    niceCount += 1

echo niceCount

file.close()
