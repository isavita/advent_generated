
fs = require 'fs'

readAll = (path) -> fs.readFileSync(path, 'utf8').trim()

num = (w) ->
  n = 0
  for i in [0...w.length]
    n *= 10
    n += w[i]
  n

main = () ->
  k = []
  l = []
  m = []

  lines = readAll("input.txt").split("\n")
  for i in [0...lines.length]
    line = lines[i]
    v = 0
    switch i % 18
      when 4
        v = parseInt(line.split(' ')[2])
        l.push v
      when 5
        v = parseInt(line.split(' ')[2])
        k.push v
      when 15
        v = parseInt(line.split(' ')[2])
        m.push v

  constraints = {}
  stack = []
  for i in [0...l.length]
    if l[i] == 1
      stack.push i
    else if l[i] == 26
      pop = stack.pop()
      constraints[pop] = [i, m[pop] + k[i]]

  minVal = (0 for _ in [0...14])
  for i in [0...14]
    if not constraints[i]?
      continue
    vmin = 1
    while vmin + constraints[i][1] < 1
      vmin += 1
    minVal[i] = vmin
    minVal[constraints[i][0]] = vmin + constraints[i][1]

  console.log num(minVal)

main()
