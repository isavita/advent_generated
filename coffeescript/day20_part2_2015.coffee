fs = require 'fs'

# Optimized function to find the first house meeting the new target presents criteria
findFirstHousePartTwo = (targetPresents) ->
  # Initialize an array to hold the number of presents received by each house
  # The array size is based on a heuristic for efficiency
  limit = Math.floor(targetPresents / 11)
  presents = new Array(limit + 1).fill(0)

  # Populate the presents array with the number of presents each elf delivers
  # considering the new limit of 50 houses per elf
  for elf in [1..limit]
    maxHouses = Math.min(50, Math.floor(limit / elf))
    for house in [1..maxHouses]
      houseIndex = elf * house
      presents[houseIndex] += elf * 11 if houseIndex <= limit

  # Find the first house that meets or exceeds the target number of presents
  house = presents.findIndex((p) -> p >= targetPresents)

  house

# Read the target number of presents from input.txt and solve for Part Two
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return

  targetPresents = parseInt(data.trim())
  firstHouse = findFirstHousePartTwo(targetPresents)

  if firstHouse > 0
    console.log "Under the new rules, the first house to get at least #{targetPresents} presents is house #{firstHouse}."
  else
    console.log "No house meets the target presents with the given algorithm adjustments for Part Two."
