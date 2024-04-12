fs = require 'fs'

readAll = (path) ->
  fs.readFileSync(path, 'utf8').trim()

num = (w) ->
  n = 0
  for v in w
    n = n * 10 + v
  n

main = ->
  input = readAll 'input.txt'
  lines = input.split '\n'
  k = []
  l = []
  m = []
  for line, i in lines
    switch i % 18
      when 4
        v = parseInt line.split(' ')[2]
        l.push v
      when 5
        v = parseInt line.split(' ')[2]
        k.push v
      when 15
        v = parseInt line.split(' ')[2]
        m.push v

  constraints = {}
  stack = []
  for i in [0...l.length]
    switch l[i]
      when 1
        stack.push i
      when 26
        pop = stack.pop()
        constraints[pop] = [i, m[pop] + k[i]]

  max = Array(14).fill(0)
  for i in [0...14]
    continue unless constraints[i]?
    vmax = 9
    vmax-- while vmax + constraints[i][1] > 9
    max[i] = vmax
    max[constraints[i][0]] = vmax + constraints[i][1]

  console.log num(max)

main()