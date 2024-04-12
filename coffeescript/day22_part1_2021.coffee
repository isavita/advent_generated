fs = require 'fs'

class RebootStep
  constructor: (@action, @xStart, @xEnd, @yStart, @yEnd, @zStart, @zEnd) ->

parseRebootStep = (line) ->
  parts = line.split " "
  action = parts[0]
  parts = parts[1].split ","
  xRange = parts[0].substring(2).split ".."
  yRange = parts[1].substring(2).split ".."
  zRange = parts[2].substring(2).split ".."
  new RebootStep action, parseInt(xRange[0]), parseInt(xRange[1]), parseInt(yRange[0]), parseInt(yRange[1]), parseInt(zRange[0]), parseInt(zRange[1])

createCubeGrid = (minCoord, maxCoord) ->
  gridSize = maxCoord - minCoord + 1
  Array(gridSize).fill(0).map -> Array(gridSize).fill(0).map -> Array(gridSize).fill(false)

executeRebootSteps = (cubeGrid, rebootSteps) ->
  for step in rebootSteps
    continue unless step.xStart >= -50 and step.xEnd <= 50 and step.yStart >= -50 and step.yEnd <= 50 and step.zStart >= -50 and step.zEnd <= 50
    for x in [step.xStart..step.xEnd]
      for y in [step.yStart..step.yEnd]
        for z in [step.zStart..step.zEnd]
          cubeGrid[x+50][y+50][z+50] = if step.action is "on" then true else false

countOnCubes = (cubeGrid) ->
  count = 0
  for x in cubeGrid
    for y in x
      for z in y
        count++ if z
  count

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  lines = data.trim().split '\n'
  rebootSteps = (parseRebootStep line for line in lines when line isnt "")
  minCoord = -50
  maxCoord = 50
  cubeGrid = createCubeGrid minCoord, maxCoord
  executeRebootSteps cubeGrid, rebootSteps
  onCubes = countOnCubes cubeGrid
  console.log onCubes