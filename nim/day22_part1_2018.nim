import strutils, sequtils, sugar

proc parseInput(data: string): (int, array[2, int]) =
  let lines = data.splitLines()
  let depth = parseInt(lines[0].split()[1])
  let coords = lines[1].split()[1].split(',')
  let x = parseInt(coords[0])
  let y = parseInt(coords[1])
  return (depth, [x, y])

proc makeCaveSystem(depth: int, target: array[2, int]): seq[seq[int]] =
  result = newSeqWith(target[1] + 1, newSeq[int](target[0] + 1))
  for y in 0 .. target[1]:
    for x in 0 .. target[0]:
      var geologicIndex: int
      if x == 0 and y == 0 or (x == target[0] and y == target[1]):
        geologicIndex = 0
      elif y == 0:
        geologicIndex = x * 16807
      elif x == 0:
        geologicIndex = y * 48271
      else:
        geologicIndex = result[y][x-1] * result[y-1][x]
      result[y][x] = (geologicIndex + depth) mod 20183

proc calculateRiskLevel(cave: seq[seq[int]], target: array[2, int]): int =
  for y in 0 .. target[1]:
    for x in 0 .. target[0]:
      result += cave[y][x] mod 3

when isMainModule:
  let data = readFile("input.txt")
  let (depth, target) = parseInput(data)
  let cave = makeCaveSystem(depth, target)
  let riskLevel = calculateRiskLevel(cave, target)
  echo riskLevel