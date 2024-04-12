fs = require 'fs'

class Component
  constructor: (@a, @b) ->

maxStrength = 0

findStrongestBridge = (components, used, port, strength) ->
  maxStrength = strength if strength > maxStrength
  for i in [0...components.length]
    continue if used[i]
    c = components[i]
    if c.a == port or c.b == port
      used[i] = true
      nextPort = if c.a == port then c.b else c.a
      findStrongestBridge components, used, nextPort, strength + c.a + c.b
      used[i] = false

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  components = []
  lines = data.trim().split "\n"
  for line in lines
    [a, b] = line.split('/').map (x) -> parseInt x, 10
    components.push new Component a, b

  used = new Array(components.length).fill false
  findStrongestBridge components, used, 0, 0
  console.log maxStrength