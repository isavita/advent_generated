
fs = require 'fs'

lines = fs.readFileSync('input.txt','utf8')
          .replace(/\r/g,'')
          .split('\n')
height = lines.length
width = Math.max.apply null, lines.map (l) -> l.length

sx = sy = 0
found = false
for y in [0...height] when !found
  line = lines[y]
  for x in [0...line.length] when line[x] == 'S'
    sx = x; sy = y; found = true; break

active = new Uint8Array width
active[sx] = 1
splits = 0

y = sy
while y < height
  next = new Uint8Array width
  row = lines[y] or ''
  for x in [0...width]
    continue unless active[x]
    c = row[x] or ' '
    if c == '^'
      splits++
      next[x-1] = 1 if x > 0
      next[x+1] = 1 if x + 1 < width
    else
      next[x] = 1
  active = next
  break unless active.some (v) -> v
  y++

console.log splits
