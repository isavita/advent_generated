fs = require 'fs'

# Optimized function to find the first house meeting the target presents
findFirstHouse = (targetPresents) ->
  # Initialize an array to hold the number of presents received by each house
  # We use a heuristic to define the size of this array to reduce computation
  # The heuristic is based on the problem's observation and may need adjustment for different inputs
  limit = Math.floor(targetPresents / 10)
  presents = new Array(limit + 1).fill(0)

  # Populate the presents array with the number of presents each elf delivers
  for elf in [1..limit]
    for house in [elf..limit] by elf
      presents[house] += elf * 10
      # Break early if a house has reached the target presents (optional optimization)
      break if presents[house] >= targetPresents

  # Find the first house that meets or exceeds the target number of presents
  house = presents.findIndex((p) -> p >= targetPresents)
  
  house

# Read the target number of presents from input.txt and solve
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return

  targetPresents = parseInt(data.trim())
  firstHouse = findFirstHouse(targetPresents)

  if firstHouse > 0
    console.log "The first house to get at least #{targetPresents} presents is house #{firstHouse}."
  else
    console.log "No house meets the target presents with the given algorithm adjustments."
