import strutils, sequtils, math

type Reindeer = object
  speed: int
  flyTime: int
  restTime: int
  distance: int
  flyCount: int
  restCount: int
  isFlying: bool

proc parseReindeer(line: string): Reindeer =
  let parts = line.split()
  let speed = parseInt(parts[3])
  let flyTime = parseInt(parts[6])
  let restTime = parseInt(parts[13])
  Reindeer(speed: speed, flyTime: flyTime, restTime: restTime, distance: 0, flyCount: 0, restCount: 0, isFlying: true)

proc updateReindeer(reindeer: var Reindeer) =
  if reindeer.isFlying:
    reindeer.distance += reindeer.speed
    reindeer.flyCount += 1
    if reindeer.flyCount == reindeer.flyTime:
      reindeer.isFlying = false
      reindeer.restCount = 0
  else:
    reindeer.restCount += 1
    if reindeer.restCount == reindeer.restTime:
      reindeer.isFlying = true
      reindeer.flyCount = 0

proc solve() =
  let lines = readFile("input.txt").splitLines()
  var reindeers: seq[Reindeer] = lines.map(parseReindeer)
  for _ in 1..2503:
    for reindeer in reindeers.mitems:
      updateReindeer(reindeer)
  let maxDistance = reindeers.mapIt(it.distance).max
  echo maxDistance

when isMainModule:
  solve()