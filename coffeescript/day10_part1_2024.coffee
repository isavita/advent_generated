
fs = require 'fs'

data = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
nr = data.length
nc = data[0].length
grid = (row.split('').map(Number) for row in data)

dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]]
trailheads = []
for r in [0...nr]
  for c in [0...nc]
    if grid[r][c] is 0
      trailheads.push [r, c]

sumScores = 0
for th in trailheads
  reached = {}
  front = [[th, 0]]
  visited = {}
  while front.length > 0
    [cur, h] = front.pop()
    if h is 9
      reached[cur.join(',')] = true unless reached[cur.join(',')]
      continue
    for [dr, dc] in dirs
      nr2 = cur[0] + dr
      nc2 = cur[1] + dc
      if nr2 < 0 or nr2 >= nr or nc2 < 0 or nc2 >= nc
        continue
      if grid[nr2][nc2] is h + 1
        key = [nr2, nc2, h + 1].join(',')
        if not visited[key]
          visited[key] = true
          front.push [[nr2, nc2], h + 1]
  sumScores += Object.keys(reached).length

console.log sumScores
