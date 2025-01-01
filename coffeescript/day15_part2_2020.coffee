
fs = require 'fs'

data = fs.readFileSync('input.txt', 'utf-8').trim().split(',')
spoken = {}
lastSpoken = null

for num, i in data
  if i == data.length - 1
    lastSpoken = parseInt(num)
  else
    spoken[parseInt(num)] = i + 1

for turn in [data.length + 1..30000000]
  if spoken[lastSpoken]?
    nextNumber = turn - 1 - spoken[lastSpoken]
  else
    nextNumber = 0
  spoken[lastSpoken] = turn - 1
  lastSpoken = nextNumber

console.log lastSpoken
