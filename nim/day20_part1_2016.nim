import sequtils, strutils, algorithm

type
  Range = tuple[start: int, endVal: int]  # Renamed 'end' to 'endVal'

proc parseRange(s: string): Range =
  let parts = s.split('-')
  (start: parseInt(parts[0]), endVal: parseInt(parts[1]))

proc main() =
  let input = readFile("input.txt").splitLines()
  var ranges: seq[Range] = @[]
  for line in input:
    ranges.add parseRange(line)

  ranges.sort(proc (a, b: Range): int = cmp(a.start, b.start))

  var lowestUnblocked: int = 0
  for r in ranges:
    if r.start > lowestUnblocked:
      break
    lowestUnblocked = max(lowestUnblocked, r.endVal + 1)

  echo "Lowest unblocked IP: ", lowestUnblocked

when isMainModule:
  main()