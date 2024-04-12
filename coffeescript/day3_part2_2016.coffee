fs = require 'fs'

isValidTriangle = (a, b, c) -> a + b > c && a + c > b && b + c > a

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  
  numbers = []
  data.trim().split('\n').forEach (line) ->
    row = (parseInt(side) for side in line.trim().split(/\s+/))
    numbers.push row
  
  validTriangles = 0
  for i in [0...numbers[0].length]
    for j in [0...numbers.length] by 3
      if j + 2 < numbers.length and isValidTriangle numbers[j][i], numbers[j+1][i], numbers[j+2][i]
        validTriangles += 1
  
  console.log validTriangles