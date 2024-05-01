import strutils, sequtils, strformat

type RebootStep = object
  action: string
  xStart, xEnd, yStart, yEnd, zStart, zEnd: int

proc parseRebootStep(line: string): RebootStep =
  let parts = line.split(" ")
  let action = parts[0]
  let ranges = parts[1].split(",")
  let xRange = ranges[0][2..^1].split("..")
  let yRange = ranges[1][2..^1].split("..")
  let zRange = ranges[2][2..^1].split("..")
  let xStart = parseInt(xRange[0])
  let xEnd = parseInt(xRange[1])
  let yStart = parseInt(yRange[0])
  let yEnd = parseInt(yRange[1])
  let zStart = parseInt(zRange[0])
  let zEnd = parseInt(zRange[1])
  RebootStep(action: action, xStart: xStart, xEnd: xEnd, yStart: yStart, yEnd: yEnd, zStart: zStart, zEnd: zEnd)

proc createCubeGrid(minCoord, maxCoord: int): seq[seq[seq[bool]]] =
  let gridSize = maxCoord - minCoord + 1
  result = newSeq[seq[seq[bool]]](gridSize)
  for i in 0..<gridSize:
    result[i] = newSeq[seq[bool]](gridSize)
    for j in 0..<gridSize:
      result[i][j] = newSeq[bool](gridSize)

proc executeRebootSteps(cubeGrid: var seq[seq[seq[bool]]], rebootSteps: seq[RebootStep]) =
  for step in rebootSteps:
    if step.xStart < -50 or step.xEnd > 50 or step.yStart < -50 or step.yEnd > 50 or step.zStart < -50 or step.zEnd > 50:
      continue
    for x in step.xStart..step.xEnd:
      for y in step.yStart..step.yEnd:
        for z in step.zStart..step.zEnd:
          cubeGrid[x+50][y+50][z+50] = step.action == "on"

proc countOnCubes(cubeGrid: seq[seq[seq[bool]]]): int =
  result = 0
  for i in 0..<cubeGrid.len:
    for j in 0..<cubeGrid[i].len:
      for k in 0..<cubeGrid[i][j].len:
        if cubeGrid[i][j][k]:
          result.inc

when isMainModule:
  let file = readFile("input.txt")
  let lines = file.splitLines()
  var rebootSteps: seq[RebootStep]
  for line in lines:
    if line == "":
      continue
    rebootSteps.add parseRebootStep(line)
  let minCoord = -50
  let maxCoord = 50
  var cubeGrid = createCubeGrid(minCoord, maxCoord)
  executeRebootSteps(cubeGrid, rebootSteps)
  let onCubes = countOnCubes(cubeGrid)
  echo onCubes