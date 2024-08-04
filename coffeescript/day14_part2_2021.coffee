fs = require 'fs'

readInput = (filename) ->
  data = fs.readFileSync filename, 'utf8'
  lines = data.split('\n')
  template = lines[0]
  rules = {}
  for line in lines[2..]
    if line
      [pair, insert] = line.split(' -> ')
      rules[pair] = insert
  [template, rules]

calculateDifference = (template, rules) ->
  pairCounts = {}
  for i in [0...template.length - 1]
    pair = template[i..i + 1]
    pairCounts[pair] ?= 0
    pairCounts[pair]++

  for step in [0...40]
    newPairCounts = {}
    for pair, count of pairCounts
      insert = rules[pair]
      if insert
        newPairCounts[pair[0] + insert] ?= 0
        newPairCounts[pair[0] + insert] += count
        newPairCounts[insert + pair[1]] ?= 0
        newPairCounts[insert + pair[1]] += count
      else
        newPairCounts[pair] ?= 0
        newPairCounts[pair] += count
    pairCounts = newPairCounts

  elementCounts = {}
  for pair, count of pairCounts
    elementCounts[pair[0]] ?= 0
    elementCounts[pair[0]] += count
  elementCounts[template[template.length - 1]] ?= 0
  elementCounts[template[template.length - 1]]++

  maxCount = Math.max(...Object.values(elementCounts))
  minCount = Math.min(...Object.values(elementCounts))
  maxCount - minCount

[template, rules] = readInput 'input.txt'
console.log calculateDifference template, rules