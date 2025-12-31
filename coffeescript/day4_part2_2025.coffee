
fs = require 'fs'
lines = fs.readFileSync('input.txt','utf8').split /\n/
grid = (l.replace(/\r$/,'').split '' for l in lines when l.length)
R = grid.length
C = 0
for row in grid
  C = row.length if row.length > C
if R is 0 or C is 0
  console.log "Total rolls removed: 0"
  process.exit 0

removed = 0
dr = [-1,-1,-1,0,0,1,1,1]
dc = [-1,0,1,-1,1,-1,0,1]

getChar = (r,c) ->
  return '' if r<0 or r>=R or c<0
  return '' if c>=grid[r].length
  grid[r][c]

changed = true
while changed
  changed = false
  for r in [0...R]
    for c in [0...C]
      continue unless getChar(r,c) is '@'
      cnt = 0
      for k in [0...8]
        cnt++ if getChar(r+dr[k],c+dc[k]) is '@'
      if cnt < 4
        grid[r][c] = '*'
        changed = true
  for r in [0...R]
    for c in [0...C]
      if getChar(r,c) is '*'
        grid[r][c] = '.'
        ++removed

console.log "Total rolls removed: #{removed}"
