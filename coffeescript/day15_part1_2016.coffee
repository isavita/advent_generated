fs = require 'fs'

class Disc
  constructor: (@totalPositions, @startPosition) ->

checkDiscs = (discs, time) ->
  for disc, i in discs
    position = (disc.startPosition + time + i + 1) % disc.totalPositions
    return false if position != 0
  true

discs = []
discRegex = /Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)./

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  lines = data.split '\n'
  for line in lines
    matches = line.match discRegex
    if matches
      totalPositions = parseInt(matches[2], 10)
      startPosition = parseInt(matches[3], 10)
      discs.push new Disc(totalPositions, startPosition)

  time = 0
  loop
    if checkDiscs(discs, time)
      console.log time
      break
    time++