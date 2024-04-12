fs = require 'fs'

applyInsertion = (polymer, rules) ->
  newPolymer = []
  for i in [0...polymer.length - 1]
    newPolymer.push polymer[i]
    pair = polymer[i..i+1]
    newPolymer.push rules[pair] if pair of rules
  newPolymer.push polymer[polymer.length - 1]
  newPolymer.join ''

countElements = (polymer) ->
  counts = {}
  for c in polymer
    counts[c] = (counts[c] or 0) + 1
  counts

minMax = (counts) ->
  values = (v for k, v of counts)
  [Math.min(values...), Math.max(values...)]

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  [polymer, ...rulesData] = data.trim().split '\n'
  rules = {}
  for line in rulesData
    continue if line.trim() is ''
    [pair, insert] = line.split ' -> '
    rules[pair] = insert

  for step in [1..10]
    polymer = applyInsertion polymer, rules

  counts = countElements polymer
  [min, max] = minMax counts
  console.log max - min