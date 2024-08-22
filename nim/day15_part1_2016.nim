import strutils

type Disc = tuple[positions: int, start: int]

proc parseInput(filename: string): seq[Disc] =
  result = @[]
  let file = readFile(filename)
  for line in file.splitLines():
    let parts = line.split(" ")
    let positions = parseInt(parts[3])
    let start = parseInt(parts[11][0..<parts[11].len-1])
    result.add((positions, start))

proc findFirstTime(discs: seq[Disc]): int =
  var time = 0
  while true:
    var allAligned = true
    for i, disc in discs:
      if (disc.start + time + i + 1) mod disc.positions != 0:
        allAligned = false
        break
    if allAligned:
      return time
    inc time

let discs = parseInput("input.txt")
echo findFirstTime(discs)