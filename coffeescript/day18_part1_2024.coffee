
fs = require 'fs'

size = 71
grid = (()->
  grid = []
  for _ in [0...size]
    grid.push (()->
      row = []
      for _ in [0...size]
        row.push false
      row
    )()
  grid
)()

try
  data = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
  for i in [0...Math.min(1024, data.length)]
    parts = data[i].split(',')
    x = parseInt(parts[0])
    y = parseInt(parts[1])
    if x >= 0 and x < size and y >= 0 and y < size
      grid[y][x] = true
catch error
  console.error error
  process.exit(1)

dirs = [[1,0],[-1,0],[0,1],[0,-1]]
visited = (()->
  visited = []
  for _ in [0...size]
    visited.push (()->
      row = []
      for _ in [0...size]
        row.push false
      row
    )()
  visited
)()

q = [{x:0,y:0,steps:0}]
visited[0][0] = true

while q.length > 0
  cur = q.shift()
  if cur.x == size-1 and cur.y == size-1
    console.log cur.steps
    process.exit(0)
  for d in dirs
    nx = cur.x + d[0]
    ny = cur.y + d[1]
    if nx>=0 and ny>=0 and nx<size and ny<size and not grid[ny][nx] and not visited[ny][nx]
      visited[ny][nx]=true
      q.push {x:nx,y:ny,steps:cur.steps+1}
console.log "No path"
