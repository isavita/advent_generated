
fs = require 'fs'

readInputFile = (callback) ->
  fs.readFile 'input.txt', 'utf8', (err, data) ->
    if err
      console.error "Error reading file: #{err}"
      return
    callback data.split('\n')

calculateMaxSeatID = (inputs) ->
  maxSeatID = 0
  for input in inputs
    binary = input.replace /F/g, '0'
             .replace /B/g, '1'
             .replace /L/g, '0'
             .replace /R/g, '1'
    row = parseInt(binary.slice(0, 7), 2)
    column = parseInt(binary.slice(7), 2)
    seatID = row * 8 + column
    if seatID > maxSeatID
      maxSeatID = seatID
  maxSeatID

decodeBinary = (binaryStr) ->
  result = 0
  for i in [0...binaryStr.length]
    char = binaryStr[i]
    if char == '1'
      result |= 1 << (binaryStr.length - i - 1)
  result

readInputFile (inputs) ->
  console.log calculateMaxSeatID(inputs)
