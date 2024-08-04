fs = require 'fs'

hexToBin = (hex) ->
  bin = ''
  for h in hex
    b = parseInt(h, 16).toString(2).padStart(4, '0')
    bin += b
  bin

parsePacket = (binStr, idx) ->
  version = (binStr[idx] - '0') * 4 + (binStr[idx + 1] - '0') * 2 + (binStr[idx + 2] - '0')
  typeID = (binStr[idx + 3] - '0') * 4 + (binStr[idx + 4] - '0') * 2 + (binStr[idx + 5] - '0')
  idx += 6

  if typeID == 4
    value = 0
    while binStr[idx] == '1'
      value = (value * 16) + parseInt(binStr.slice(idx + 1, idx + 5), 2)
      idx += 5
    value = (value * 16) + parseInt(binStr.slice(idx + 1, idx + 5), 2)
    idx += 5
    return [version, idx, value]

  lengthTypeID = binStr[idx] - '0'
  idx++
  subPacketLength = 0
  numSubPackets = 0

  if lengthTypeID == 0
    subPacketLength = parseInt(binStr.slice(idx, idx + 15), 2)
    idx += 15
  else
    numSubPackets = parseInt(binStr.slice(idx, idx + 11), 2)
    idx += 11

  values = []
  while true
    if lengthTypeID == 0 and subPacketLength == 0
      break
    if lengthTypeID == 1 and numSubPackets == 0
      break
    [_, newIndex, subValue] = parsePacket(binStr, idx)
    values.push(subValue)

    if lengthTypeID == 0
      subPacketLength -= newIndex - idx
    else
      numSubPackets--
    idx = newIndex

  result = 0
  switch typeID
    when 0
      result = values.reduce ((a, b) -> a + b), 0
    when 1
      result = values.reduce ((a, b) -> a * b), 1
    when 2
      result = Math.min(values...)
    when 3
      result = Math.max(values...)
    when 5
      result = if values[0] > values[1] then 1 else 0
    when 6
      result = if values[0] < values[1] then 1 else 0
    when 7
      result = if values[0] == values[1] then 1 else 0

  [version, idx, result]

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    process.exit(1)

  hexStr = data.trim()
  binStr = hexToBin(hexStr)
  [_, _, value] = parsePacket(binStr, 0)
  console.log value