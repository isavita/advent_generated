fs = require 'fs'

class Instruction
  constructor: (@left, @right) ->

ElemToMatch = 'ZZZ'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  input = data.trim()

  re = /[A-Z]{3}/g

  lines = input.split '\n'

  desertMap = {}

  for line in lines[2..] when line.length > 0
    matches = line.match re
    desertMap[matches[0]] = new Instruction matches[1], matches[2]

  current = 'AAA'
  steps = 0

  while current isnt ElemToMatch
    for direction in lines[0].trim()
      if direction is 'R'
        current = desertMap[current].right
      else if direction is 'L'
        current = desertMap[current].left
      steps++

      break if current is ElemToMatch

  console.log steps