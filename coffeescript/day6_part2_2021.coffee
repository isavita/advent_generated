fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  lanternFishCounts = Array(9).fill(0)
  fishAges = data.trim().split ','

  for age in fishAges
    lanternFishCounts[parseInt age]++

  for i in [0...256]
    newLanternFish = lanternFishCounts[0]
    for j in [0...8]
      lanternFishCounts[j] = lanternFishCounts[j+1] if j < 8
    lanternFishCounts[6] += newLanternFish
    lanternFishCounts[8] = newLanternFish

  total = lanternFishCounts.reduce ((acc, num) -> acc + num), 0
  console.log total