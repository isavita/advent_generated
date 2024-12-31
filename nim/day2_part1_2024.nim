
import strutils, sequtils, math, os

proc parseLevels(line: string): seq[int] =
  for field in line.split():
    result.add(parseInt(field))

proc isSafeReport(levels: seq[int]): bool =
  if levels.len < 2:
    return false
  let firstDiff = levels[1] - levels[0]
  if firstDiff == 0:
    return false
  let isIncreasing = firstDiff > 0
  for i in 0..<levels.len - 1:
    let diff = levels[i + 1] - levels[i]
    if diff == 0:
      return false
    if (isIncreasing and diff <= 0) or (not isIncreasing and diff >= 0):
      return false
    let absDiff = abs(diff)
    if absDiff < 1 or absDiff > 3:
      return false
  return true

var safeReportCount = 0
for line in lines("input.txt"):
  let levels = parseLevels(line)
  if isSafeReport(levels):
    safeReportCount += 1
echo safeReportCount
