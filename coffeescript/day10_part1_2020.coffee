
fs = require 'fs'

readFile = (callback) ->
  fs.readFile 'input.txt', 'utf8', (err, data) ->
    if err
      console.error "Error reading file: #{err}"
      return
    callback data.split('\n').map (num) -> parseInt(num, 10)

readFile (adapters) ->
  adapters.sort (a, b) -> a - b
  joltDifferences = {1: 0, 3: 1}
  previousJoltage = 0

  for adapter in adapters
    diff = adapter - previousJoltage
    joltDifferences[diff]++
    previousJoltage = adapter

  console.log joltDifferences[1] * joltDifferences[3]
