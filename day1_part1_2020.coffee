
fs = require('fs')
readline = require('readline')

fileStream = fs.createReadStream('input.txt')
rl = readline.createInterface({ input: fileStream })

numbers = []

rl.on('line', (line) ->
  if line == ''
    return
  n = parseInt(line)
  numbers.push n
)

rl.on('close', () ->
  for i in [0...numbers.length - 1]
    for j in [i + 1...numbers.length]
      if numbers[i] + numbers[j] == 2020
        console.log numbers[i] * numbers[j]
        return
)
