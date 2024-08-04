fs = require 'fs'

class RangeMap
  constructor: (@srcStart, @destStart, @length) ->

reverseConvertNumber = (number, ranges) ->
  for i in [ranges.length - 1..0]
    r = ranges[i]
    if number >= r.destStart and number < r.destStart + r.length
      return r.srcStart + (number - r.destStart)
  number

isInSeedRanges = (number, ranges) ->
  for r in ranges
    if number >= r[0] and number < r[0] + r[1]
      return true
  false

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  lines = data.split '\n'
  seedRanges = []
  currentRanges = []
  maps = []

  for line in lines
    if line.includes 'map:'
      if currentRanges.length > 0
        maps.push currentRanges
        currentRanges = []
    else if line.startsWith 'seeds:'
      seedStrs = line.slice(7).split ' '
      for i in [0...seedStrs.length] by 2
        start = parseInt seedStrs[i]
        length = parseInt seedStrs[i + 1]
        seedRanges.push [start, length]
    else
      numbers = line.split /\s+/
      if numbers.length == 3
        srcStart = parseInt numbers[1]
        destStart = parseInt numbers[0]
        length = parseInt numbers[2]
        currentRanges.push new RangeMap srcStart, destStart, length

  if currentRanges.length > 0
    maps.push currentRanges

  location = 0
  while true
    seed = location
    for i in [maps.length - 1..0]
      seed = reverseConvertNumber seed, maps[i]
    if isInSeedRanges seed, seedRanges
      console.log location
      break
    location++