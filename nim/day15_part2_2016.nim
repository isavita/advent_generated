import strutils, sequtils

type Disc = tuple[positions: int, start: int]

proc parseInput(filename: string): seq[Disc] =
  for line in lines(filename):
    let parts = line.split(" ")
    let positions = parseInt(parts[3])
    let start = parseInt(parts[11][0..^2])
    result.add((positions, start))

proc findFirstTime(discs: seq[Disc]): int =
  var time = 0
  while true:
    var allAligned = true
    for i, disc in discs:
      if (time + i + 1 + disc.start) mod disc.positions != 0:
        allAligned = false
        break
    if allAligned:
      return time
    inc time

let discs = parseInput("input.txt")
echo "Part 1: ", findFirstTime(discs)

let newDiscs = discs & @[(11, 0)]
echo "Part 2: ", findFirstTime(newDiscs)