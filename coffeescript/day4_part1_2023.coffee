
fs = require 'fs'

try
  input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
  totalPoints = 0
  for line in input
    [winning, your] = line.split(' | ').map (s) -> s.trim().split(/\s+/).map(Number)
    points = 0
    winningSet = new Set(winning)
    for num in your
      if winningSet.has(num)
        points = if points == 0 then 1 else points * 2
    totalPoints += points
  console.log totalPoints
catch error
  console.error "Error:", error
