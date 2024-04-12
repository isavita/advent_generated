fs = require 'fs'

class Coord
  constructor: (@x, @y, @z) ->

class Brick
  constructor: (@mini, @maxi) ->
    @basedOn = []
    @support = []

parseInput = (input) ->
  bricks = []
  for line, i in input
    parts = line.split(/,|~/).map (num) -> parseInt num
    mini = new Coord parts[0], parts[1], parts[2]
    maxi = new Coord parts[3], parts[4], parts[5]
    brick = new Brick mini, maxi
    bricks.push brick
  bricks

max = (a, b) -> Math.max a, b
min = (a, b) -> Math.min a, b

settle = (bricks) ->
  bricks.sort (a, b) -> a.maxi.z - b.maxi.z
  for brick, i in bricks
    supportZ = 0
    basedBricks = []
    for j in [0...i].reverse()
      isIntersectingX = max(brick.mini.x, bricks[j].mini.x) <= min(brick.maxi.x, bricks[j].maxi.x)
      isIntersectingY = max(brick.mini.y, bricks[j].mini.y) <= min(brick.maxi.y, bricks[j].maxi.y)
      isIntersecting = isIntersectingX && isIntersectingY
      if isIntersecting
        if bricks[j].maxi.z == supportZ
          basedBricks.push bricks[j]
        else if bricks[j].maxi.z > supportZ
          supportZ = bricks[j].maxi.z
          basedBricks = [bricks[j]]
    brick.basedOn = basedBricks
    for basedBrick in basedBricks
      basedBrick.support.push brick
    deltaZ = brick.maxi.z - brick.mini.z
    brick.mini.z = supportZ + 1
    brick.maxi.z = brick.mini.z + deltaZ

solve = (input) ->
  bricks = parseInput input
  settle bricks
  cnt = 0
  for brick in bricks
    isDisintegratable = true
    for supportedBrick in brick.support
      if supportedBrick.basedOn.length < 2
        isDisintegratable = false
        break
    cnt++ if isDisintegratable
  cnt

readFile = (fileName) ->
  file = fs.readFileSync fileName, 'utf8'
  file.trim().split '\n'

main = ->
  input = readFile 'input.txt'
  console.log solve input

main()