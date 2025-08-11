import std/[os, strutils, sequtils, strformat]

const
  sizeX = 101
  sizeY = 103
  maxRobots = 1000

type
  Robot = object
    x, y: int
    vx, vy: int

proc modPos(a, b: int): int =
  ((a mod b) + b) mod b

proc parseLine(line: string): Robot =
  var parts = line.splitWhitespace()
  # parts[0] = "p=...,..."
  # parts[1] = "v=...,..."
  var p = parts[0][2..^1].split(',')   # remove "p=" and split
  var v = parts[1][2..^1].split(',')   # remove "v=" and split
  result.x = parseInt(p[0])
  result.y = parseInt(p[1])
  result.vx = parseInt(v[0])
  result.vy = parseInt(v[1])

proc moveRobots(robots: var seq[Robot]) =
  for i in 0..<robots.len:
    robots[i].x = modPos(robots[i].x + robots[i].vx, sizeX)
    robots[i].y = modPos(robots[i].y + robots[i].vy, sizeY)

proc countQuadrants(robots: seq[Robot]): int =
  var counts = [0,0,0,0]
  let centerX = sizeX div 2
  let centerY = sizeY div 2
  for r in robots:
    if r.x < centerX:
      if r.y < centerY: inc counts[0]
      elif r.y > centerY: inc counts[1]
    elif r.x > centerX:
      if r.y < centerY: inc counts[2]
      elif r.y > centerY: inc counts[3]
  result = 1
  for c in counts:
    result *= c

proc hasNoOverlaps(robots: seq[Robot]): bool =
  var occupied = newSeq[seq[bool]](sizeX)
  for i in 0..<sizeX:
    occupied[i] = newSeq[bool](sizeY)
  for r in robots:
    if occupied[r.x][r.y]:
      return false
    occupied[r.x][r.y] = true
  true

proc drawGrid(robots: seq[Robot]) =
  var grid = newSeq[seq[char]](sizeY)
  for y in 0..<sizeY:
    grid[y] = newSeq[char](sizeX)
    for x in 0..<sizeX:
      grid[y][x] = '.'
  for r in robots:
    grid[r.y][r.x] = '#'
  for y in 0..<sizeY:
    echo grid[y].join("")

proc main() =
  var robots: seq[Robot] = @[]
  for line in lines("input.txt"):
    if line.len > 0:
      robots.add parseLine(line)

  var robotsPart1 = robots
  for _ in 0..<100:
    moveRobots(robotsPart1)
  let safetyFactor = countQuadrants(robotsPart1)
  echo &"Part 1 - Safety Factor after 100 seconds: {safetyFactor}"

  var robotsPart2 = robots
  var seconds = 0
  while not hasNoOverlaps(robotsPart2):
    moveRobots(robotsPart2)
    inc seconds
    if seconds > 1_000_000:
      echo "Exceeded maximum iterations without finding a unique position configuration."
      quit 1
  echo &"Part 2 - Fewest seconds to display Easter egg: {seconds}"
  echo "Final positions of robots:"
  drawGrid(robotsPart2)

when isMainModule:
  main()
