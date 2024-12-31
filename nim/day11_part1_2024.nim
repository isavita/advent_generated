
import strutils, sequtils, os, algorithm

proc evenDigits(s: string): bool =
  s.len mod 2 == 0

proc trimLeadingZeros(s: string): string =
  var res = s
  while res.len > 1 and res[0] == '0':
    res = res[1..res.high]
  return res

proc solve() =
  let data = readFile("input.txt").strip()
  var stones = data.split()

  for _ in 0..<25:
    var next: seq[string] = @[]
    for s in stones:
      if s == "0":
        next.add("1")
      elif evenDigits(s):
        let mid = s.len div 2
        var left = trimLeadingZeros(s[0..<mid])
        var right = trimLeadingZeros(s[mid..s.high])
        if left.len == 0: left = "0"
        if right.len == 0: right = "0"
        next.add(left)
        next.add(right)
      else:
        let n = parseInt(s)
        next.add($ (n * 2024))
    stones = next

  echo stones.len

solve()
