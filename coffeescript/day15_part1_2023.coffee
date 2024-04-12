fs = require 'fs'

hashTableSize = 256

hashString = (str) ->
  res = 0
  for char, i in str
    res += char.charCodeAt(0)
    res *= 17
    res %= hashTableSize
  res

parseStep = (stepStr) ->
  step = {}
  step.Label = stepStr.replace /[=-\d]+$/, ''
  step.NumBox = hashString(step.Label)
  step.Operation = stepStr[step.Label.length]
  if step.Operation == '='
    step.Number = parseInt stepStr[(step.Label.length + 1)..]
  step

solve = (input) ->
  line = input[0]
  steps = line.split ','
  res = 0
  for step in steps
    res += hashString(step)
  res

readFile = (fileName) ->
  file = fs.readFileSync fileName, 'utf8'
  file.trim().split '\n'

main = ->
  input = readFile 'input.txt'
  console.log solve(input)

main()