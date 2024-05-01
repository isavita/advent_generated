import strutils, sequtils

proc calculateFuel(mass: int): int =
  let fuel = (mass div 3) - 2
  if fuel <= 0: 0 else: fuel + calculateFuel(fuel)

proc main() =
  let input = readFile("input.txt").splitLines().map(parseInt)
  let totalFuel = foldl(input.map(calculateFuel), a + b)
  echo totalFuel

when isMainModule:
  main()