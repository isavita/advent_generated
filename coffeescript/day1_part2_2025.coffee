
fs = require 'fs'

floorDiv = (a, b) ->
  if a >= 0n then a / b else -((-a + b - 1n) / b)

main = ->
  lines = fs.readFileSync('input.txt','utf8').split('\n')
  cur = 50n
  total = 0n
  for line in lines
    line = line.trim()
    continue if line.length is 0
    dir = line[0]
    dist = BigInt line.slice 1
    if dir is 'R'
      nxt = cur + dist
      total += floorDiv(nxt, 100n) - floorDiv(cur, 100n)
      cur = nxt
    else
      nxt = cur - dist
      total += floorDiv(cur - 1n, 100n) - floorDiv(nxt - 1n, 100n)
      cur = nxt
  console.log total.toString()

main()
