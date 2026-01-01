
fs = require 'fs'

main = ->
  data = fs.readFileSync('input.txt', 'utf8').trim()
  return console.log 0 if data.length is 0
  tokens = data.split /\s+/
  pts = ( (t.split(',').map (v) -> parseInt v, 10) for t in tokens )
  n = pts.length
  best = 0
  for i in [0...n]
    x1 = pts[i][0]
    y1 = pts[i][1]
    for j in [i...n]
      dx = Math.abs(x1 - pts[j][0]) + 1
      dy = Math.abs(y1 - pts[j][1]) + 1
      area = dx * dy
      best = area if area > best
  console.log best

main()
