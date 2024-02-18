
fs = require 'fs'
readline = require('readline')

invalidNumber = 14360655
min = max = sum = 0
numbers = []

readlineInterface = readline.createInterface({
  input: fs.createReadStream('input.txt'),
  console: false
})

readlineInterface.on('line', (line) ->
  numbers.push parseInt(line)
)

readlineInterface.on('close', ->
  for i in [0...numbers.length]
    sum = min = max = numbers[i]
    for j in [i+1...numbers.length]
      sum += numbers[j]
      min = Math.min(min, numbers[j])
      max = Math.max(max, numbers[j])
      if sum == invalidNumber
        console.log min + max
        process.exit()
      else if sum > invalidNumber
        break
)
