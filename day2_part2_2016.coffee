
fs = require 'fs'
path = require 'path'

inputPath = path.join(__dirname, 'input.txt')

keypad =
  1: {'D': '3'}
  2: {'R': '3', 'D': '6'}
  3: {'U': '1', 'R': '4', 'D': '7', 'L': '2'}
  4: {'L': '3', 'D': '8'}
  5: {'R': '6'}
  6: {'U': '2', 'R': '7', 'D': 'A', 'L': '5'}
  7: {'U': '3', 'R': '8', 'D': 'B', 'L': '6'}
  8: {'U': '4', 'R': '9', 'D': 'C', 'L': '7'}
  9: {'L': '8'}
  A: {'U': '6', 'R': 'B'}
  B: {'U': '7', 'R': 'C', 'D': 'D', 'L': 'A'}
  C: {'U': '8', 'L': 'B'}
  D: {'U': 'B'}

code = ''
position = '5'

fs.readFile inputPath, 'utf8', (err, data) ->
  if err
    console.error err
  else
    instructions = data.split('\n')

    for instruction in instructions
      for move in instruction
        if nextPos = keypad[position][move]
          position = nextPos

      code += position

    console.log code
