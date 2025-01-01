
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
  label = stepStr.match(/^[a-z]+/)[0]
  numBox = hashString(label)
  operation = stepStr[label.length]
  number = parseInt(stepStr.substring(label.length + 1)) if operation is '='
  {label, numBox, operation, number}

getBoxes = (stepsStr) ->
  boxes = {}
  for stepStr in stepsStr
    {label, numBox, operation, number} = parseStep stepStr
    boxContents = boxes[numBox] ? []
    switch operation
      when '-'
        for content, i in boxContents
          if content[label]?
            boxContents.splice i, 1
            break
      when '='
        found = false
        for content in boxContents
          if content[label]?
            content[label] = number
            found = true
            break
        if not found
          boxContents.push { [label]: number }
    if boxContents.length is 0
      delete boxes[numBox]
    else
      boxes[numBox] = boxContents
  boxes

calculatePower = (boxes) ->
  res = 0
  for iBox of boxes
    iBox = parseInt iBox
    for content, iSlot in boxes[iBox]
      for _, value of content
        res += (iBox + 1) * (iSlot + 1) * value
  res

solve = (input) ->
  line = input[0]
  stepsStr = line.split ','
  boxes = getBoxes stepsStr
  calculatePower boxes

readFile = (fileName) ->
  fs.readFileSync(fileName, 'utf-8').trim().split '\n'

input = readFile 'input.txt'
console.log solve input
