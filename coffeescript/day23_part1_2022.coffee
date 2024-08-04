fs = require 'fs'

class P
  constructor: (@X, @Y) ->

class Elf
  constructor: (@Pos) ->
    @Moving = false
    @NextPos = null

N = 1
E = 3
S = 5
W = 7

Map = {}
Elves = []
Order = [N, S, W, E]
CurrDir = 0
Dirs = [
  new P(-1, -1) # NW
  new P(-1, 0)  # N
  new P(-1, +1) # NE
  new P(0, +1)  # E
  new P(+1, +1) # SE
  new P(+1, 0)  # S
  new P(+1, -1) # SW
  new P(0, -1)  # W
]

Elf::AroundAllEmpty = ->
  for d in Dirs
    adj = new P(@Pos.X + d.X, @Pos.Y + d.Y)
    return false if Map[adj.X + ',' + adj.Y]?
  true

Elf::ElfInDirection = (wannaGo) ->
  for j in [-1..1]
    dxy = Dirs[(wannaGo + j + 8) % 8]
    adj = new P(@Pos.X + dxy.X, @Pos.Y + dxy.Y)
    return true if Map[adj.X + ',' + adj.Y]?
  false

run = ->
  proposes = {}

  for e in Elves
    continue if e.AroundAllEmpty()

    for i in [0..3]
      dir = Order[(CurrDir + i) % 4]
      continue if e.ElfInDirection(dir)

      dxy = Dirs[dir]
      dest = new P(e.Pos.X + dxy.X, e.Pos.Y + dxy.Y)
      proposes[dest.X + ',' + dest.Y] = (proposes[dest.X + ',' + dest.Y] or 0) + 1
      e.NextPos = dest
      e.Moving = true
      break

  someoneMoved = false

  for e in Elves
    continue unless e.Moving

    if proposes[e.NextPos.X + ',' + e.NextPos.Y] > 1
      e.Moving = false
      continue

    someoneMoved = true
    delete Map[e.Pos.X + ',' + e.Pos.Y]
    Map[e.NextPos.X + ',' + e.NextPos.Y] = true
    e.Pos = e.NextPos
    e.Moving = false

  CurrDir = (CurrDir + 1) % 4
  someoneMoved

minMax = ->
  min = new P(1 << 30, 1 << 30)
  max = new P(-(1 << 30), -(1 << 30))

  for p of Map
    [x, y] = p.split(',').map(Number)
    min.X = x if x < min.X
    min.Y = y if y < min.Y
    max.X = x if x > max.X
    max.Y = y if y > max.Y

  [min, max]

parse = ->
  input = fs.readFileSync('input.txt', 'utf8').split('\n')
  for line, row in input
    for char, col in line
      if char is '#'
        p = new P(row, col)
        Map[p.X + ',' + p.Y] = true
        Elves.push new Elf(p)

parse()

for i in [0..9]
  run()

[min, max] = minMax()

count = 0
for x in [min.X..max.X]
  for y in [min.Y..max.Y]
    count++ unless Map[x + ',' + y]?

console.log count