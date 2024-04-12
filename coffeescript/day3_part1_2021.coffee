fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  lines = data.trim().split '\n'
  counts = (Array(2).fill(0) for _ in [0...12])

  for line in lines
    for i in [0...line.length]
      counts[i][line[i] - '0']++

  gammaRate = 0
  epsilonRate = 0
  for i in [0...counts.length]
    if counts[i][0] > counts[i][1]
      gammaRate |= 1 << (counts.length - i - 1)
    else
      epsilonRate |= 1 << (counts.length - i - 1)

  console.log gammaRate * epsilonRate