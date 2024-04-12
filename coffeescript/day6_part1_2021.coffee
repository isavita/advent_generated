fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  return console.log "Error reading file: #{err}" if err

  fishes = (0 for i in [0...9])
  for fishStr in data.split(',')
    fish = parseInt fishStr.trim()
    fishes[fish]++

  for day in [1..80]
    newFish = fishes[0]
    for i in [1...fishes.length]
      fishes[i - 1] = fishes[i]
    fishes[6] += newFish
    fishes[8] = newFish

  totalFish = fishes.reduce ((sum, num) -> sum + num), 0

  console.log totalFish