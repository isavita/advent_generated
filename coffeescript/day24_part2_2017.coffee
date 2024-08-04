fs = require 'fs'

Component = (a, b) ->
  @a = a
  @b = b

maxStrength = 0
maxLength = 0

findStrongestLongestBridge = (components, used, port, strength, length) ->
  if length > maxLength or (length == maxLength and strength > maxStrength)
    maxStrength = strength
    maxLength = length

  for c, i in components
    continue if used[i]

    if c.a == port or c.b == port
      used[i] = true
      nextPort = if c.a == port then c.b else c.a
      findStrongestLongestBridge components, used, nextPort, strength + c.a + c.b, length + 1
      used[i] = false

input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
components = for line in input
  [a, b] = line.split('/').map((x) -> parseInt(x, 10))
  new Component(a, b)

used = new Array(components.length).fill(false)
findStrongestLongestBridge components, used, 0, 0, 0

console.log maxStrength