fs = require 'fs'

class Coord
  constructor: (@x, @y, @z) ->

class Brick
  constructor: (mini, maxi) ->
    @mini = mini
    @maxi = maxi
    @basedOn = []
    @support = []

parseInput = (input) ->
  bricks = []
  for line in input when line.trim()
    parts = line.split '~'
    a = parts[0].split ','
    b = parts[1].split ','
    x1 = parseInt(a[0], 10)
    y1 = parseInt(a[1], 10)
    z1 = parseInt(a[2], 10)
    x2 = parseInt(b[0], 10)
    y2 = parseInt(b[1], 10)
    z2 = parseInt(b[2], 10)
    bricks.push new Brick(new Coord(x1, y1, z1), new Coord(x2, y2, z2))
  bricks

settle = (bricks) ->
  bricks.sort (a, b) -> a.maxi.z - b.maxi.z
  for brick, i in bricks
    supportZ = 0
    basedBricks = []
    for j in [i - 1..0] by -1
      bbrick = bricks[j]
      if Math.max(brick.mini.x, bbrick.mini.x) <= Math.min(brick.maxi.x, bbrick.maxi.x) and
         Math.max(brick.mini.y, bbrick.mini.y) <= Math.min(brick.maxi.y, bbrick.maxi.y)
        if bbrick.maxi.z is supportZ
          basedBricks.push bbrick
        else if bbrick.maxi.z > supportZ
          supportZ = bbrick.maxi.z
          basedBricks = [bbrick]
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
    fallingBricks = new Set()
    for supportedBrick in brick.support
      if supportedBrick.basedOn.length is 1
        queue = [supportedBrick]
        while queue.length
          curr = queue.shift()
          isFalling = true
          for basedBrick in curr.basedOn
            if basedBrick isnt brick and not fallingBricks.has(basedBrick)
              isFalling = false
              break
          if isFalling and not fallingBricks.has(curr)
            fallingBricks.add curr
            queue = queue.concat curr.support
    cnt += fallingBricks.size
  cnt

fs.readFile 'input.txt', 'utf8', (err, data) ->
  return process.exit(1) if err
  lines = data.trim().split /\r?\n/
  console.log solve lines