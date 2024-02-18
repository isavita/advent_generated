
fs = require 'fs'

readInputFile = (callback) ->
  fs.readFile 'input.txt', 'utf8', (err, data) ->
    if err
      console.error "Error reading file: #{err}"
      return
    callback data

parseRange = (rangeStr) ->
  [parseInt(rangeStr.split('-')[0]), parseInt(rangeStr.split('-')[1])]

checkOverlap = (left, right) ->
  left[0] <= right[1] and left[1] >= right[0]

countOverlappingPairs = (inputStr) ->
  inputLines = inputStr.split('\n')
  count = 0
  for line in inputLines
    if line.length > 0
      pair = line.split(',')
      left = parseRange(pair[0].trim())
      right = parseRange(pair[1].trim())
      if checkOverlap(left, right)
        count++
  return count

main = ->
  readInputFile (inputStr) ->
    console.log countOverlappingPairs(inputStr)

main()
