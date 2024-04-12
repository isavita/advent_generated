fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  input = data.trim()
  ans = solve input
  console.log ans

solve = (input) ->
  lines = parseInput input
  total = 0
  for line in lines
    total += doMaths line, calcFlatSlicePart
  total

parseInput = (input) ->
  lines = input.split '\n'
  lines.map (l) -> l.replace(/\s+/g, '').split ''

doMaths = (input, flatteningFunc) ->
  stackOpenIndices = []
  stackFlattened = []
  for i in [0...input.length]
    stackFlattened.push input[i]
    switch input[i]
      when '('
        stackOpenIndices.push stackFlattened.length - 1
      when ')'
        openIndex = stackOpenIndices.pop()
        sliToFlatten = stackFlattened[openIndex + 1...stackFlattened.length - 1]
        stackFlattened[openIndex] = flatteningFunc sliToFlatten
        stackFlattened = stackFlattened[0...openIndex + 1]
  toInt flatteningFunc stackFlattened

calcFlatSlicePart = (input) ->
  throw "unexpected paren in flat input, #{input}" if '(' in input or ')' in input
  i = 1
  while i < input.length - 1
    if input[i] == '+'
      toLeft = input[i - 1]
      toRight = input[i + 1]
      if isNum(toLeft) and isNum(toRight)
        input[i - 1] = addStrings toLeft, toRight
        input.splice i, 2
        i--
    i++
  i = 1
  while i < input.length - 1
    if input[i] == '*'
      toLeft = input[i - 1]
      toRight = input[i + 1]
      if isNum(toLeft) and isNum(toRight)
        input[i - 1] = multiplyStrings toLeft, toRight
        input.splice i, 2
        i--
    i++
  input[0]

isNum = (str) -> /^[0-9]+$/.test str

addStrings = (str...) ->
  sum = 0
  sum += parseInt(s) for s in str
  sum.toString()

multiplyStrings = (str...) ->
  product = 1
  product *= parseInt(s) for s in str
  product.toString()

toInt = (s) -> parseInt s