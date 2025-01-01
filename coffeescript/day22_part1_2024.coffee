
fs = require 'fs'

nextSecret = (s) ->
  s ^= s * 64
  s &= 0xFFFFFF
  s ^= Math.floor(s / 32)
  s &= 0xFFFFFF
  s ^= s * 2048
  s &= 0xFFFFFF
  s

buyers = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').map(Number)

total = 0
for b in buyers
  s = b
  for i in [0...2000]
    s = nextSecret(s)
  total += s

console.log total
