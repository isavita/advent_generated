
fs = require 'fs'

trimLeadingZeros = (s) ->
  i = 0
  i++ while i < s.length - 1 and s[i] is '0'
  s.substring i

splitStone = (s) ->
  mid = Math.floor s.length / 2
  left = trimLeadingZeros s.substring 0, mid
  right = trimLeadingZeros s.substring mid
  left = '0' if left is ''
  right = '0' if right is ''
  [left, right]

multiplyBy2024 = (s) ->
  num = s.split('').map Number
  multiplier = [2, 0, 2, 4]
  result = (0 for _ in [0..num.length + multiplier.length - 1])
  i = num.length - 1
  while i >= 0
    carry = 0
    j = multiplier.length - 1
    while j >= 0
      product = num[i] * multiplier[j] + result[i + j + 1] + carry
      result[i + j + 1] = product % 10
      carry = Math.floor product / 10
      j--
    result[i] += carry
    i--
  start = 0
  start++ while start < result.length - 1 and result[start] is 0
  result.slice(start).join ''

main = ->
  try
    input = fs.readFileSync('input.txt', 'utf-8').trim()
  catch error
    console.log "Error reading input.txt:", error
    return
  stonesStr = input.split /\s+/
  stonesMap = {}
  for s in stonesStr
    stonesMap[s] = (stonesMap[s] or 0) + 1
  steps = 75
  step = 0
  while step < steps
    newStonesMap = {}
    for stone, count of stonesMap
      if stone is '0'
        newStonesMap['1'] = (newStonesMap['1'] or 0) + count
      else if stone.length % 2 is 0
        [left, right] = splitStone stone
        newStonesMap[left] = (newStonesMap[left] or 0) + count
        newStonesMap[right] = (newStonesMap[right] or 0) + count
      else
        newStone = multiplyBy2024 stone
        newStonesMap[newStone] = (newStonesMap[newStone] or 0) + count
    stonesMap = newStonesMap
    step++
  totalStones = 0
  for _, count of stonesMap
    totalStones += count
  console.log totalStones

main()
