fs = require 'fs'

diskLength = 272

fs.readFile 'input.txt', 'utf8', (err, initialState) ->
  throw err if err
  data = generateData initialState, diskLength
  checksum = calculateChecksum data
  console.log "Checksum:", checksum

generateData = (initialState, length) ->
  data = initialState
  while data.length < length
    b = []
    i = data.length - 1
    while i >= 0
      if data[i] == '0'
        b.push '1'
      else
        b.push '0'
      i--
    data += "0" + b.join ''
  data.slice 0, length

calculateChecksum = (data) ->
  while data.length % 2 == 0
    b = []
    i = 0
    while i < data.length
      if data[i] == data[i + 1]
        b.push '1'
      else
        b.push '0'
      i += 2
    data = b.join ''
  data