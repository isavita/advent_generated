fs = require 'fs'

repeatInput = (input, times) ->
  digits = []
  for t in [0...times]
    for char, i in input
      digits.push parseInt(char)
  digits

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  # Prepare the repeated input signal
  repeatedInput = repeatInput data.trim(), 10000

  # Find the message offset
  offset = parseInt data.slice(0, 7)

  # Apply FFT for 100 phases starting from the offset
  for phase in [0...100]
    sum = 0
    for i in [repeatedInput.length - 1...offset - 1] by -1
      sum += repeatedInput[i]
      repeatedInput[i] = sum % 10

  # Print the eight-digit message
  message = repeatedInput.slice(offset, offset + 8).join('')
  console.log message