
import strutils, sequtils, algorithm, os

proc canMake(design: string, patterns: seq[string]): bool =
  var n = design.len
  var dp = newSeq[bool](n + 1)
  dp[0] = true
  for i in 1..n:
    for p in patterns:
      var lp = p.len
      if i >= lp and dp[i - lp] and design[i - lp ..< i] == p:
        dp[i] = true
        break
  return dp[n]

var f = open("input.txt")
var availableLine = f.readLine()
var availablePatterns = availableLine.split(",").map(proc(x: string): string = x.strip())
discard f.readLine()

var count = 0
for line in f.lines:
  if canMake(line, availablePatterns):
    count += 1
echo count
f.close()
