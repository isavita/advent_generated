import strutils, sequtils

proc calculatePowerLevel(x, y, serialNumber: int): int =
  let rackId = x + 10
  var powerLevel = rackId * y
  powerLevel += serialNumber
  powerLevel *= rackId
  powerLevel = (powerLevel mod 1000) div 100
  powerLevel -= 5
  return powerLevel

proc findLargestSquare(serialNumber: int): (int, int) =
  var maxPower = -100
  var maxCoord: (int, int)
  for x in 1..298:
    for y in 1..298:
      var power = 0
      for dx in 0..2:
        for dy in 0..2:
          power += calculatePowerLevel(x + dx, y + dy, serialNumber)
      if power > maxPower:
        maxPower = power
        maxCoord = (x, y)
  return maxCoord

when isMainModule:
  let input = readFile("input.txt").strip.parseInt
  let (x, y) = findLargestSquare(input)
  echo "The X,Y coordinate of the top-left fuel cell of the 3x3 square with the largest total power is: ", x, ",", y