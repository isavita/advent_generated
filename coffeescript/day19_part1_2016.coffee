fs = require 'fs'

findWinningElf = (totalElves) ->
  highestPowerOfTwo = 1
  highestPowerOfTwo *= 2 while highestPowerOfTwo * 2 <= totalElves
  (totalElves - highestPowerOfTwo) * 2 + 1

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  totalElves = parseInt(data.trim())
  winner = findWinningElf(totalElves)
  console.log winner