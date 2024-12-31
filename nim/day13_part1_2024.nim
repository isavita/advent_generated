
import strutils, sequtils, math

type Button = tuple[x: int, y: int]
type Prize = tuple[x: int, y: int]
type Machine = tuple[a: Button, b: Button, prize: Prize]

proc parseLine(line: string): Machine =
  let parts = line.split("\n")
  let aParts = parts[0].split(", ")
  let bParts = parts[1].split(", ")
  let prizeParts = parts[2].split(", ")

  let aX = parseInt(aParts[0].split("+")[1])
  let aY = parseInt(aParts[1].split("+")[1])
  let bX = parseInt(bParts[0].split("+")[1])
  let bY = parseInt(bParts[1].split("+")[1])
  let prizeX = parseInt(prizeParts[0].split("=")[1])
  let prizeY = parseInt(prizeParts[1].split("=")[1])

  return (a: (x: aX, y: aY), b: (x: bX, y: bY), prize: (x: prizeX, y: prizeY))

proc solve(machines: seq[Machine]): (int, int) =
  var totalTokens = 0
  var prizesWon = 0

  for machine in machines:
    var minTokens = int.high
    for aCount in 0..100:
      for bCount in 0..100:
        let x = machine.a.x * aCount + machine.b.x * bCount
        let y = machine.a.y * aCount + machine.b.y * bCount
        if x == machine.prize.x and y == machine.prize.y:
          let tokens = aCount * 3 + bCount
          minTokens = min(minTokens, tokens)
    if minTokens != int.high:
      totalTokens += minTokens
      prizesWon += 1
  return (prizesWon, totalTokens)

proc main() =
  let lines = readFile("input.txt").split("\n\n")
  let machines = lines.map(parseLine)
  let (prizesWon, totalTokens) = solve(machines)
  echo totalTokens

main()
