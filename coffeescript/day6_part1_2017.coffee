fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log 'File reading error', err
    return

  banks = (parseInt(num) for num in data.trim().split /\s+/)
  seen = {}
  cycles = 0

  while true
    state = banks.join(',')

    if seen[state]
      break
    seen[state] = true

    maxIndex = 0
    for i in [1...banks.length]
      maxIndex = i if banks[i] > banks[maxIndex]

    blocks = banks[maxIndex]
    banks[maxIndex] = 0
    for i in [1..blocks]
      banks[(maxIndex + i) % banks.length]++

    cycles++

  console.log "It takes #{cycles} redistribution cycles to reach a repeated configuration."