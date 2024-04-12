fs = require 'fs'
{ deepEqual } = require 'assert'

compare = (a, b) ->
  if typeof a is 'number' and typeof b is 'number'
    return sign(a - b)
  else if typeof a is 'number'
    return compare [a], b
  else if typeof b is 'number'
    return compare a, [b]
  else
    for i in [0...Math.min(a.length, b.length)]
      c = compare a[i], b[i]
      return c if c isnt 0
    return sign a.length - b.length

sign = (n) ->
  return 0 if n is 0
  return -1 if n < 0
  1

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  packets = []
  sum = 0
  pairs = data.split '\n\n'
  for pair, i in pairs
    [first, second] = pair.split '\n'
    first = JSON.parse first
    second = JSON.parse second
    packets.push first, second
    sum += i + 1 if compare(first, second) is -1
  console.log sum