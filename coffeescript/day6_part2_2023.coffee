fs = require 'fs'

calculateWaysToWinLongRace = (time, record) ->
  waysToWin = 0
  for holdTime in [1...time]
    travelTime = time - holdTime
    distance = holdTime * travelTime
    waysToWin++ if distance > record
  waysToWin

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return

  lines = data.trim().split '\n'
  time = parseInt lines[0].split(':')[1].replace(/\s/g, '')
  distance = parseInt lines[1].split(':')[1].replace(/\s/g, '')

  waysToWin = calculateWaysToWinLongRace time, distance
  console.log waysToWin