fs = require 'fs'

class RangeMap
  constructor: (@srcStart, @destStart, @length) ->

convertNumber = (number, ranges) ->
  for r in ranges
    if number >= r.srcStart and number < r.srcStart + r.length
      return r.destStart + (number - r.srcStart)
  number

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    throw err

  lines = data.split '\n'
  seeds = []
  currentRanges = []
  maps = []

  for line in lines
    if line.includes 'map:'
      if currentRanges.length > 0
        maps.push currentRanges
        currentRanges = []
    else if line.startsWith 'seeds:'
      seedStrs = line.slice(7).split ' '
      seeds.push parseInt(s) for s in seedStrs
    else
      numbers = line.trim().split /\s+/
      if numbers.length == 3
        [destStart, srcStart, length] = numbers.map (n) -> parseInt(n)
        currentRanges.push new RangeMap srcStart, destStart, length

  maps.push currentRanges

  minLocation = -1
  for seed in seeds
    location = seed
    for m in maps
      location = convertNumber location, m

    if minLocation == -1 or location < minLocation
      minLocation = location

  console.log minLocation