fs = require 'fs'
crypto = require 'crypto'

reverseSection = (arr, start, length) ->
  n = arr.length
  i = start
  j = start + length - 1
  while i < j
    temp = arr[i % n]
    arr[i % n] = arr[j % n]
    arr[j % n] = temp
    i++
    j--

knotHash = (input) ->
  lengths = []
  for char in input
    lengths.push char.charCodeAt(0)
  lengths.push 17, 31, 73, 47, 23

  list = [0...256]

  position = 0
  skip = 0
  for round in [0...64]
    for length in lengths
      reverseSection list, position, length
      position = (position + length + skip) % 256
      skip++

  denseHash = []
  for i in [0...16]
    xor = 0
    for j in [0...16]
      xor ^= list[i*16 + j]
    denseHash.push xor

  hexHash = Buffer.from(denseHash).toString('hex')
  return hexHash

hexToBinary = (hexStr) ->
  binaryStr = ''
  for hexDigit in hexStr
    val = parseInt hexDigit, 16
    binaryStr += val.toString(2).padStart(4, '0')
  return binaryStr

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log 'File reading error', err
    return

  keyString = data.trim()
  totalUsed = 0

  for i in [0...128]
    rowKey = "#{keyString}-#{i}"
    hash = knotHash rowKey
    binaryRow = hexToBinary hash

    for bit in binaryRow
      if bit == '1'
        totalUsed++

  console.log totalUsed