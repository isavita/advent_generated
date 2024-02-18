
fs = require 'fs'

readInput = (callback) ->
  fs.readFile 'input.txt', 'utf8', (err, data) ->
    if err
      console.error "Error reading file: #{err}"
      return
    rows = data.split('\n')
    callback rows

decodePass = (pass) ->
  row = parseInt(pass.slice(0, 7).replace(/F/g, '0').replace(/B/g, '1'), 2)
  column = parseInt(pass.slice(7).replace(/L/g, '0').replace(/R/g, '1'), 2)
  row * 8 + column

findMissingSeat = (seatIDs) ->
  for i in [0...seatIDs.length - 1]
    if seatIDs[i + 1] != seatIDs[i] + 1
      console.log seatIDs[i] + 1
      return

readInput (rows) ->
  seatIDs = rows.map decodePass
  seatIDs.sort (a, b) -> a - b
  findMissingSeat seatIDs
