import strutils, sequtils

type
  Range = tuple[start: int, finish: int]

proc parseRange(s: string): Range =
  let parts = s.split('-')
  (start: parseInt(parts[0]), finish: parseInt(parts[1]))

proc fullyContains(a, b: Range): bool =
  (a.start <= b.start and a.finish >= b.finish) or
  (b.start <= a.start and b.finish >= a.finish)

proc overlaps(a, b: Range): bool =
  not (a.finish < b.start or a.start > b.finish)

proc main =
  let input = readFile("input.txt").splitLines()
  var fullyContained = 0
  var overlapping = 0

  for line in input:
    let parts = line.split(',')
    let range1 = parseRange(parts[0])
    let range2 = parseRange(parts[1])

    if fullyContains(range1, range2) or fullyContains(range2, range1):
      fullyContained.inc

    if overlaps(range1, range2):
      overlapping.inc

  echo "Fully contained: ", fullyContained
  echo "Overlapping: ", overlapping

when isMainModule:
  main()