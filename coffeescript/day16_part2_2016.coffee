fs = require 'fs'

diskLength = 35651584 # Disk length for the problem

readInitialState = (filename) ->
  fs.readFileSync(filename, 'utf8').trim()

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
    data = data + '0' + b.join('')
  data[...length]

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
    data = b.join('')
  data

initialState = readInitialState 'input.txt'
data = generateData initialState, diskLength
checksum = calculateChecksum data
console.log "Checksum:", checksum