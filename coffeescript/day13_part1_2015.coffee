fs = require 'fs'

# Function to parse input and return happiness map
parseInput = (data) ->
  happinessChanges = {}
  for line in data.trim().split('\n')
    [_, person1, gainLose, units, person2] = /^(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.$/.exec(line)
    units = parseInt(units)
    units *= -1 if gainLose == 'lose'
    happinessChanges[person1] ?= {}
    happinessChanges[person1][person2] = units
  happinessChanges

# Function to generate all permutations of the guest list
permute = (arr) ->
  permutations = []
  permuteHelper = (arr, m = []) ->
    if arr.length == 0
      permutations.push(m)
    else
      for i in [0...arr.length]
        newArr = arr.slice(0, i).concat(arr.slice(i+1))
        permuteHelper(newArr, m.concat(arr[i]))
  permuteHelper(arr)
  permutations

# Function to calculate total happiness for a seating arrangement
calculateHappiness = (arrangement, happinessChanges) ->
  totalHappiness = 0
  for i in [0...arrangement.length]
    left = arrangement[(i - 1 + arrangement.length) % arrangement.length]
    right = arrangement[(i + 1) % arrangement.length]
    person = arrangement[i]
    totalHappiness += happinessChanges[person][left] + happinessChanges[person][right]
  totalHappiness

# Main function to find the optimal seating arrangement
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return

  happinessChanges = parseInput(data)
  guests = Object.keys(happinessChanges)
  permutations = permute(guests)
  maxHappiness = Math.max(permutations.map((arrangement) -> calculateHappiness(arrangement, happinessChanges))...)

  console.log maxHappiness
