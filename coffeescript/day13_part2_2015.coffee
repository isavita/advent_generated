fs = require 'fs'

# Parse input and build a map of happiness scores
parseInput = (input) ->
  scores = {}
  for line in input.trim().split('\n')
    parts = line.match /^(.*) would (gain|lose) (\d+) happiness units by sitting next to (.*).$/
    [_, person1, change, units, person2] = parts
    units = parseInt(units)
    units = -units if change == 'lose'
    scores[person1] ?= {}
    scores[person1][person2] = units
  scores

# Add yourself to the scores map with 0 happiness impact
addYourself = (scores) ->
  for person in Object.keys(scores)
    scores[person]['You'] = 0
    scores['You'] ?= {}
    scores['You'][person] = 0
  scores

# Calculate the total happiness for a seating arrangement
calculateHappiness = (arrangement, scores) ->
  total = 0
  for i in [0...arrangement.length]
    left = arrangement[i]
    right = arrangement[(i + 1) % arrangement.length]
    total += scores[left][right] + scores[right][left]
  total

# Generate all permutations of an array
permute = (arr) ->
  permutations = []
  generate = (n, arr) ->
    if n == 1
      permutations.push(arr.slice())
    else
      for i in [0...n]
        generate(n - 1, arr)
        if n % 2 == 0
          [arr[i], arr[n - 1]] = [arr[n - 1], arr[i]] # swap
        else
          [arr[0], arr[n - 1]] = [arr[n - 1], arr[0]] # swap
  generate(arr.length, arr)
  permutations

# Read input, add yourself, find the optimal arrangement
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return

  scores = parseInput(data)
  scores = addYourself(scores)
  guests = Object.keys(scores)
  bestHappiness = -Infinity

  for arrangement in permute(guests)
    happiness = calculateHappiness(arrangement, scores)
    bestHappiness = Math.max(bestHappiness, happiness)

  console.log "The total change in happiness for the optimal arrangement is: #{bestHappiness}"
