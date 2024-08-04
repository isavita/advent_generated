fs = require 'fs'

readAll = (path) ->
  fs.readFileSync(path).toString()

abs = (x) ->
  if x < 0 then -x else x

x = [1]
for line in readAll('input.txt').split('\n')
  if line == 'noop'
    x.push(x[x.length - 1])
  else
    [_, n] = line.match(/addx (-?\d+)/)
    n = parseInt(n)
    x.push(x[x.length - 1])
    x.push(x[x.length - 1] + n)

grid = {}
for i in [0...x.length]
  crtx = i % 40
  crty = Math.floor(i / 40)
  if abs(crtx - x[i]) <= 1
    grid["#{crtx},#{crty}"] = true
  else
    delete grid["#{crtx},#{crty}"]

for y in [0...6]
  for x in [0...40]
    if grid["#{x},#{y}"]
      process.stdout.write('#')
    else
      process.stdout.write('.')
  console.log()