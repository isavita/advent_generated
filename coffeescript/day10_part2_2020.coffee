fs = require 'fs'

countArrangements = (adapters) ->
  ways = {}
  ways[0] = 1
  for i in [1...adapters.length]
    currentJoltage = adapters[i]
    for diff in [1, 2, 3]
      ways[currentJoltage] = (ways[currentJoltage] or 0) + (ways[currentJoltage - diff] or 0)
  ways[adapters[adapters.length - 1]]

fs.readFile 'input.txt', 'utf8', (err, data) ->
  return console.log "Error opening file: #{err}" if err
  adapters = [0]
  for line in data.trim().split '\n'
    joltage = parseInt line
    adapters.push joltage
  adapters.sort (a, b) -> a - b
  adapters.push adapters[adapters.length - 1] + 3
  console.log countArrangements adapters