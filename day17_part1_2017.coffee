
fs = require('fs')

data = fs.readFileSync('input.txt', 'utf8').trim()
steps = parseInt(data)
buffer = [0]
currentPos = 0

for i in [1...2018]
  currentPos = (currentPos + steps) % buffer.length
  buffer.splice(currentPos + 1, 0, i)
  currentPos++

console.log(buffer[(buffer.indexOf(2017) + 1) % buffer.length])
