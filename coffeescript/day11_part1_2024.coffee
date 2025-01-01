
fs = require 'fs'

data = fs.readFileSync('input.txt', 'utf-8').trim()
stones = data.split(/\s+/)

evenDigits = (s) -> s.length % 2 == 0

trimLeadingZeros = (s) ->
  while s.length > 1 and s[0] == '0'
    s = s.substring(1)
  s

for i in [0...25]
  next = []
  for s in stones
    if s == "0"
      next.push "1"
    else if evenDigits(s)
      mid = Math.floor(s.length / 2)
      left = trimLeadingZeros(s.substring(0, mid))
      right = trimLeadingZeros(s.substring(mid))
      left = "0" if left == ""
      right = "0" if right == ""
      next.push left, right
    else
      next.push String(parseInt(s) * 2024)
  stones = next

console.log stones.length
