fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim()
lengths = input.split('').map (c) -> c.charCodeAt(0)
lengths.push 17, 31, 73, 47, 23

list = (i for i in [0...256])
currentPosition = 0
skipSize = 0

for round in [0...64]
  for length in lengths
    for i in [0...Math.floor(length / 2)]
      start = (currentPosition + i) % 256
      end = (currentPosition + length - 1 - i) % 256
      [list[start], list[end]] = [list[end], list[start]]
    currentPosition = (currentPosition + length + skipSize) % 256
    skipSize++

denseHash = []
for i in [0...256] by 16
  xor = 0
  for j in [0...16]
    xor ^= list[i + j]
  denseHash.push xor

hexHash = denseHash.map((b) -> ('0' + b.toString(16)).slice(-2)).join('')
console.log hexHash