import strutils, sequtils, streams

type RangeMap = object
  srcStart, destStart, length: int

proc convertNumber(number: int, ranges: seq[RangeMap]): int =
  for r in ranges:
    if number >= r.srcStart and number < r.srcStart + r.length:
      return r.destStart + (number - r.srcStart)
  return number

proc main() =
  let file = newFileStream("input.txt", fmRead)
  defer: file.close()

  var seeds: seq[int]
  var currentRanges: seq[RangeMap]
  var maps: seq[seq[RangeMap]] = @[]

  for line in file.lines:
    if "map:" in line:
      if currentRanges.len > 0:
        maps.add(currentRanges)
        currentRanges = @[]
    elif line.startsWith("seeds:"):
      let seedStrs = line[7..^1].split_whitespace
      for s in seedStrs:
        seeds.add(parseInt(s))
    else:
      let numbers = line.splitWhitespace
      if numbers.len == 3:
        let srcStart = parseInt(numbers[1])
        let destStart = parseInt(numbers[0])
        let length = parseInt(numbers[2])
        currentRanges.add(RangeMap(srcStart: srcStart, destStart: destStart, length: length))

  maps.add(currentRanges)

  var minLocation = -1
  for seed in seeds:
    var location = seed
    for m in maps:
      location = convertNumber(location, m)
    if minLocation == -1 or location < minLocation:
      minLocation = location

  echo minLocation

main()