import strutils, sequtils, strformat

type RangeMap = object
  srcStart, destStart, length: int

proc reverseConvertNumber(number: int, ranges: seq[RangeMap]): int =
  for r in ranges:
    if number >= r.destStart and number < r.destStart + r.length:
      return r.srcStart + (number - r.destStart)
  return number

proc isInSeedRanges(number: int, ranges: seq[(int, int)]): bool =
  for r in ranges:
    if number >= r[0] and number < r[0] + r[1]:
      return true
  return false

when isMainModule:
  let file = open("input.txt")
  var seedRanges: seq[(int, int)]
  var currentRanges: seq[RangeMap]
  var maps: seq[seq[RangeMap]]

  for line in file.lines:
    if line.contains("map:"):
      if currentRanges.len > 0:
        maps.add(currentRanges)
        currentRanges = @[]
    elif line.startsWith("seeds:"):
      let seedStrs = line[7..^1].split(' ')
      for i in countup(0, seedStrs.len - 1, 2):
        let start = seedStrs[i].parseInt
        let length = seedStrs[i + 1].parseInt
        seedRanges.add((start, length))
    else:
      let numbers = line.split()
      if numbers.len == 3:
        let srcStart = numbers[1].parseInt
        let destStart = numbers[0].parseInt
        let length = numbers[2].parseInt
        currentRanges.add(RangeMap(srcStart: srcStart, destStart: destStart, length: length))

  if currentRanges.len > 0:
    maps.add(currentRanges)

  file.close()  # Move the file closing here

  var location = 0
  while true:
    var seed = location
    for i in countdown(maps.len - 1, 0):
      seed = reverseConvertNumber(seed, maps[i])
    if isInSeedRanges(seed, seedRanges):
      echo location
      break
    inc location