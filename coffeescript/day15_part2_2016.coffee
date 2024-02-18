
fs = require('fs')
readline = require('readline')

class Disc
  constructor: (@totalPositions, @startPosition) ->

discs = []

inputStream = fs.createReadStream('input.txt')
rl = readline.createInterface({input: inputStream})

lineNum = 0
rl.on('line', (line) ->
  match = /Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)/.exec(line)
  discs.push new Disc(parseInt(match[2]), parseInt(match[3]))
)

rl.on('close', ->
  # Add the new disc as per Part Two's requirement
  discs.push new Disc(11, 0)
  
  time = 0
  while true
    if checkDiscs(discs, time)
      console.log time
      process.exit()
    
    time++
)

checkDiscs = (discs, time) ->
  for disc in discs
    position = (disc.startPosition + time + discs.indexOf(disc) + 1) % disc.totalPositions
    if position != 0
      return false
  
  return true
