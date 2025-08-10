
import std/[strutils, os]

const
  WIDTH  = 101
  HEIGHT = 103
  MAX_ROBOTS = 1000

type
  Robot = object
    x, y, vx, vy: int

var
  robots: array[0 .. MAX_ROBOTS-1, Robot]
  robotCount = 0

# read input file
let lines = readFile("input.txt").splitLines()
for line in lines:
  if robotCount >= MAX_ROBOTS: break
  # line format: p=px,py v=vx,vy
  var parts = line.split(' ')
  if parts.len < 2: continue
  var p = parts[0][2 .. ^1]          # px,py
  var v = parts[1][2 .. ^1]          # vx,vy
  var pVals = p.split(',')
  var vVals = v.split(',')
  if pVals.len != 2 or vVals.len != 2: continue
  robots[robotCount].x = parseInt(pVals[0])
  robots[robotCount].y = parseInt(pVals[1])
  robots[robotCount].vx = parseInt(vVals[0])
  robots[robotCount].vy = parseInt(vVals[1])
  inc robotCount

# simulate 100 steps
for _ in 0 ..< 100:
  for i in 0 ..< robotCount:
    var r = robots[i]
    r.x = (r.x + r.vx) mod WIDTH
    r.y = (r.y + r.vy) mod HEIGHT
    if r.x < 0: r.x += WIDTH
    if r.y < 0: r.y += HEIGHT
    robots[i] = r

# count quadrants
var q1, q2, q3, q4: int
for i in 0 ..< robotCount:
  let r = robots[i]
  if r.x == 50 or r.y == 51: continue
  if r.x < 50 and r.y < 51: inc q1
  elif r.x > 50 and r.y < 51: inc q2
  elif r.x < 50 and r.y > 51: inc q3
  elif r.x > 50 and r.y > 51: inc q4

echo q1 * q2 * q3 * q4
