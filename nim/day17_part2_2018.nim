
import strutils, strscans

var 
  grid: array[2005, array[1000, char]]
  maxY: int

proc isSolid(x, y: int): bool =
  if y > maxY: return false
  grid[y][x] == '#' or grid[y][x] == '~'

proc fill(x, y: int) =
  if y > maxY or grid[y][x] == '#' or grid[y][x] == '~': return
  let flowing = grid[y][x] == '|'
  grid[y][x] = '|'
  
  if not flowing: 
    fill(x, y + 1)
  
  if isSolid(x, y + 1):
    var lx = x
    var lw = false
    while true:
      let nx = lx - 1
      if nx < 0: break
      if grid[y][nx] == '#': (lw = true; break)
      let f = grid[y][nx] == '|'
      grid[y][nx] = '|'
      if not f: fill(nx, y + 1)
      if not isSolid(nx, y + 1): break
      lx = nx
      
    var rx = x
    var rw = false
    while true:
      let nx = rx + 1
      if nx >= 1000: break
      if grid[y][nx] == '#': (rw = true; break)
      let f = grid[y][nx] == '|'
      grid[y][nx] = '|'
      if not f: fill(nx, y + 1)
      if not isSolid(nx, y + 1): break
      rx = nx
      
    if lw and rw:
      for i in lx..rx: grid[y][i] = '~'

let f = open("input.txt")
var line: string
while f.readLine(line):
  var a, b, c: int
  if scanf(line, "x=$i, y=$i..$i", a, b, c):
    for y in b..c: (grid[y][a] = '#'; if y > maxY: maxY = y)
  elif scanf(line, "y=$i, x=$i..$i", a, b, c):
    for x in b..c: (grid[a][x] = '#'; if a > maxY: maxY = a)
f.close()

fill(500, 0)

var res = 0
for y in 0..maxY:
  for x in 0..999:
    if grid[y][x] == '~': inc res
echo res

