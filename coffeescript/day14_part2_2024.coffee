
fs = require 'fs'

mod = (a, b) -> (a % b + b) % b

parseLine = (line) ->
  match = line.match /p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/
  if not match
    throw new Error "invalid line format: #{line}"
  [_, x, y, vx, vy] = match
  {x: parseInt(x), y: parseInt(y), vx: parseInt(vx), vy: parseInt(vy)}

moveRobots = (robots, sizeX, sizeY) ->
  for robot in robots
    robot.x = mod(robot.x + robot.vx, sizeX)
    robot.y = mod(robot.y + robot.vy, sizeY)

countQuadrants = (robots, sizeX, sizeY) ->
  counts = [0, 0, 0, 0]
  centerX = Math.floor(sizeX / 2)
  centerY = Math.floor(sizeY / 2)
  for robot in robots
    {x, y} = robot
    if x < centerX
      if y < centerY
        counts[0]++
      else if y > centerY
        counts[1]++
    else if x > centerX
      if y < centerY
        counts[2]++
      else if y > centerY
        counts[3]++
  counts

hasNoOverlaps = (robots) ->
  positions = new Set()
  for robot in robots
    pos = "#{robot.x},#{robot.y}"
    if positions.has(pos)
      return false
    positions.add(pos)
  true

drawGrid = (robots, sizeX, sizeY) ->
  gridMap = {}
  for robot in robots
    gridMap["#{robot.x},#{robot.y}"] = true
  for y in [0...sizeY]
    line = ""
    for x in [0...sizeX]
      if gridMap["#{x},#{y}"]
        line += "#"
      else
        line += "."
    console.log line

main = ->
  sizeX = 101
  sizeY = 103
  try
    input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
  catch error
    console.error "Error reading input.txt: #{error}"
    process.exit(1)
  robots = (parseLine line for line in input when line)

  robotsPart1 = robots.map (r) -> {x: r.x, y: r.y, vx: r.vx, vy: r.vy}
  for _ in [0...100]
    moveRobots robotsPart1, sizeX, sizeY
  counts = countQuadrants robotsPart1, sizeX, sizeY
  safetyFactor = counts.reduce ((a, b) -> a * b), 1
  console.log "Part 1 - Safety Factor after 100 seconds: #{safetyFactor}"

  robotsPart2 = robots.map (r) -> {x: r.x, y: r.y, vx: r.vx, vy: r.vy}
  seconds = 0
  while true
    if hasNoOverlaps robotsPart2
      break
    moveRobots robotsPart2, sizeX, sizeY
    seconds++
    if seconds > 1000000
      console.log "Exceeded maximum iterations without finding a unique position configuration."
      process.exit(1)
  console.log "Part 2 - Fewest seconds to display Easter egg: #{seconds}"
  console.log "Final positions of robots:"
  drawGrid robotsPart2, sizeX, sizeY

main()
