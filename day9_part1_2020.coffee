
fs = require 'fs'
readline = require 'readline'

preambleLength = 25
numbers = []

input = readline.createInterface({
  input: fs.createReadStream('input.txt'),
  console: false
})

input.on('line', (line) ->
  numbers.push parseInt(line)
  if numbers.length > preambleLength + 1
    number = numbers[numbers.length - 1]
    previousNumbers = numbers[numbers.length - 1 - preambleLength...numbers.length - 1]
    if not isValid(number, previousNumbers)
      console.log number
      input.close()
)

input.on('close', ->)

isValid = (number, previousNumbers) ->
  seen = {}
  for n in previousNumbers
    if seen[number - n]
      return true
    seen[n] = true
  return false
