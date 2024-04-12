fs = require 'fs'

reverseSection = (arr, start, length) ->
  n = arr.length
  for i in [0...Math.floor(length / 2)]
    left = (start + i) % n
    right = (start + length - 1 - i) % n
    [arr[left], arr[right]] = [arr[right], arr[left]]

knotHash = (inputString) ->
  lengths = (inputString.split('').map (char) -> char.charCodeAt(0)).concat([17, 31, 73, 47, 23])
  listSize = 256
  lst = [0...listSize]
  position = skip = 0

  for _ in [0...64]
    for length in lengths
      reverseSection lst, position, length
      position += length + skip
      skip += 1

  denseHash = []
  for i in [0...16]
    xor = 0
    for j in [0...16]
      xor ^= lst[i * 16 + j]
    denseHash.push xor

  denseHash.map((x) -> x.toString(16).padStart(2, '0')).join('')

hexToBinary = (hexStr) ->
  hexStr.split('').map((digit) -> parseInt(digit, 16).toString(2).padStart(4, '0')).join('')

dfs = (x, y, grid) ->
  return unless 0 <= x < 128 and 0 <= y < 128 and grid[x][y] == 1
  grid[x][y] = 0
  for [dx, dy] in [[-1, 0], [1, 0], [0, -1], [0, 1]]
    dfs x + dx, y + dy, grid

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  keyString = data.trim()

  grid = []
  totalUsed = 0
  regions = 0

  for i in [0...128]
    rowKey = "#{keyString}-#{i}"
    hashValue = knotHash rowKey
    binaryRow = hexToBinary hashValue

    row = (bit for bit in binaryRow).map (bit) -> parseInt(bit)
    grid.push row
    totalUsed += row.reduce (a, b) -> a + b

  for i in [0...128]
    for j in [0...128]
      if grid[i][j] == 1
        regions += 1
        dfs i, j, grid

  console.log "Regions: #{regions}"