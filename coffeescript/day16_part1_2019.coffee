fs = require 'fs'

applyFFT = (input) ->
  basePattern = [0, 1, 0, -1]
  output = new Array(input.length)
  for i in [0...input.length]
    sum = 0
    for j in [0...input.length]
      patternValue = basePattern[((j + 1) / (i + 1)) % basePattern.length | 0]
      sum += input[j] * patternValue
    output[i] = Math.abs(sum % 10)
  output

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    throw err
  digits = data.split('').map(Number)
  for phase in [0...100]
    digits = applyFFT(digits)
  console.log digits.slice(0, 8).join('')