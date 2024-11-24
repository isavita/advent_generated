
import strutils, sequtils, math

proc solve(): tuple[x: int, y: int, size: int] =
  let serial = parseInt(readFile("input.txt").strip)
  const gridSize = 300
  var grid = newSeqWith(gridSize, newSeqWith(gridSize, 0))

  for y in 0..<gridSize:
    for x in 0..<gridSize:
      let rackID = x + 11
      var powerLevel = rackID * (y + 1)
      powerLevel += serial
      powerLevel *= rackID
      powerLevel = (powerLevel div 100) mod 10
      powerLevel -= 5
      grid[y][x] = powerLevel

  var maxPower = -2147483648
  var maxX = 0
  var maxY = 0
  var maxSize = 0

  for size in 1..gridSize:
    var sumTable = newSeqWith(gridSize + 1, newSeqWith(gridSize + 1, 0))
    for y in 0..<gridSize:
      for x in 0..<gridSize:
        sumTable[y+1][x+1] = sumTable[y+1][x] + sumTable[y][x+1] - sumTable[y][x] + grid[y][x]

    for y in 0..<gridSize-size+1:
      for x in 0..<gridSize-size+1:
        let totalPower = sumTable[y+size][x+size] - sumTable[y+size][x] - sumTable[y][x+size] + sumTable[y][x]
        if totalPower > maxPower:
          maxPower = totalPower
          maxX = x + 1
          maxY = y + 1
          maxSize = size

  return (maxX, maxY, maxSize)


let (x, y, size) = solve()
echo x, ",", y, ",", size
