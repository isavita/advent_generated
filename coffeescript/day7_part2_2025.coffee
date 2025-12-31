
fs = require 'fs'

main = ->
  lines = fs.readFileSync('input.txt', 'utf8')
           .split /\r?\n/
           .filter (l) -> l.length > 0

  height = lines.length
  if height is 0
    console.log 0
    return

  width = lines[0].length

  startX = -1
  startY = -1
  for y in [0...height] when startX is -1
    for x in [0...width]
      if lines[y][x] is 'S'
        startX = x
        startY = y
        break

  unless startX >= 0
    console.error "Start point 'S' not found"
    process.exit 1

  counts = new Map()
  counts.set startX, 1n

  for y in [startY...height]
    next = new Map()
    counts.forEach (cnt, x) ->
      isSplitter = x >= 0 and x < width and lines[y][x] is '^'
      if isSplitter
        for k in [x - 1, x + 1]
          next.set k, (next.get(k) or 0n) + cnt
      else
        next.set x, (next.get(x) or 0n) + cnt
    counts = next

  total = 0n
  counts.forEach (cnt) -> total += cnt
  console.log total.toString()

main()
