
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8')
lines = input.split('\n').filter (line) -> line.trim() != ''
vals = lines.map (line) -> parseInt(line)

prevSum = vals[0] + vals[1] + vals[2]
count = 0

for i in [3...vals.length]
  currSum = vals[i-2] + vals[i-1] + vals[i]
  if currSum > prevSum then count++
  prevSum = currSum

console.log count
