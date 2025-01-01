
fs = require 'fs'

canReach = (grid) ->
  n = grid.length
  return false if grid[0][0] or grid[n-1][n-1]
  dirs = [[1,0],[-1,0],[0,1],[0,-1]]
  visited = (for _ in [0...n] then (for _ in [0...n] then false))
  q = [[0,0]]
  visited[0][0] = true
  while q.length > 0
    [cx,cy] = q.shift()
    return true if cx == n-1 and cy == n-1
    for [dx,dy] in dirs
      nx = cx + dx
      ny = cy + dy
      if nx >= 0 and ny >= 0 and nx < n and ny < n and not grid[ny][nx] and not visited[ny][nx]
        visited[ny][nx] = true
        q.push [nx,ny]
  false

size = 71
grid = (for _ in [0...size] then (for _ in [0...size] then false))
lines = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
for line,i in lines
  [x,y] = line.split(',').map(Number)
  if x>=0 and x<size and y>=0 and y<size
    grid[y][x] = true
  if not canReach(grid)
    console.log "#{x},#{y}"
    process.exit(0)
console.log "No cutoff found"
