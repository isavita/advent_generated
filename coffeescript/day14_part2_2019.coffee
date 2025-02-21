
fs = require 'fs'

# Parse the reactions from the input file
parseReactions = (input) ->
  reactions = {}
  for line in input.trim().split('\n')
    [inputs, output] = line.split(' => ')
    [outputAmount, outputChemical] = output.split(' ')
    reactions[outputChemical] =
      outputAmount: parseInt(outputAmount)
      inputs: {}
    for input in inputs.split(', ')
      [inputAmount, inputChemical] = input.split(' ')
      reactions[outputChemical].inputs[inputChemical] = parseInt(inputAmount)
  reactions

# Calculate the ORE required for a given amount of FUEL
calculateOre = (reactions, fuelAmount) ->
  required = { FUEL: fuelAmount }
  have = {}
  ore = 0

  while Object.keys(required).length > 0
    chemical = Object.keys(required)[0]
    amountNeeded = required[chemical]
    delete required[chemical]

    if chemical == 'ORE'
      ore += amountNeeded
      continue

    if have[chemical]? and have[chemical] >= amountNeeded
      have[chemical] -= amountNeeded
      continue

    amountHave = if have[chemical]? then have[chemical] else 0
    amountToMake = amountNeeded - amountHave
    reaction = reactions[chemical]
    multiplier = Math.ceil(amountToMake / reaction.outputAmount)

    have[chemical] = (multiplier * reaction.outputAmount) - amountToMake

    for inputChemical, inputAmount of reaction.inputs
      amountRequired = inputAmount * multiplier
      if required[inputChemical]?
          required[inputChemical] += amountRequired
      else
          required[inputChemical] = amountRequired

  ore
# Binary search function to find max fuel
binarySearch = (reactions, maxOre) ->
    low = 0
    high = maxOre
    maxFuel = 0

    while low <= high
        mid = Math.floor((low + high) / 2)
        oreNeeded = calculateOre(reactions, mid)

        if oreNeeded <= maxOre
            maxFuel = mid
            low = mid + 1
        else
            high = mid - 1
    maxFuel

# Read input from file
input = fs.readFileSync('input.txt', 'utf8')
reactions = parseReactions(input)

# Part 1
oreForOneFuel = calculateOre(reactions, 1)
console.log "Minimum ORE for 1 FUEL: #{oreForOneFuel}"

# Part 2
maxOre = 1000000000000
maxFuel = binarySearch(reactions, maxOre)

console.log "Maximum FUEL with 1 trillion ORE: #{maxFuel}"
