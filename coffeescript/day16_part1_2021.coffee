fs = require 'fs'

hexToBin = (hex) ->
  bin = ''
  for h in hex
    b = parseInt(h, 16)
    bin += b.toString(2).padStart(4, '0')
  bin

parsePacket = (binStr, idx) ->
  version = parseInt(binStr[idx...idx+3], 2)
  typeID = parseInt(binStr[idx+3...idx+6], 2)
  idx += 6

  if typeID == 4
    while binStr[idx] == '1'
      idx += 5
    idx += 5
    return [version, idx]

  lengthTypeID = parseInt(binStr[idx], 2)
  idx++
  numSubPackets = 0
  subPacketLength = 0

  if lengthTypeID == 0
    for i in [0...15]
      subPacketLength = (subPacketLength << 1) | parseInt(binStr[idx], 2)
      idx++
  else
    for i in [0...11]
      numSubPackets = (numSubPackets << 1) | parseInt(binStr[idx], 2)
      idx++

  versionSum = version
  while true
    if lengthTypeID == 0 and subPacketLength == 0
      break
    if lengthTypeID == 1 and numSubPackets == 0
      break
    [subVersion, newIndex] = parsePacket(binStr, idx)
    versionSum += subVersion

    if lengthTypeID == 0
      subPacketLength -= newIndex - idx
    else
      numSubPackets--

    idx = newIndex

  [versionSum, idx]

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return

  hexStr = data.trim()
  binStr = hexToBin(hexStr)
  [versionSum, _] = parsePacket(binStr, 0)
  console.log versionSum