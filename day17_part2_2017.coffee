
fs = require 'fs'

try
  data = fs.readFileSync('input.txt', 'utf8').trim()
  steps = parseInt(data)
  currentPos = 0
  valueAfterZero = 0

  for i in [1...50000000]
    currentPos = (currentPos + steps) % i
    if currentPos == 0
      valueAfterZero = i
    currentPos++

  console.log valueAfterZero
catch error
  console.error "File reading error: #{error}"
