
fs = require('fs')
readline = require('readline')

freq = 0

inputStream = fs.createReadStream('input.txt')
rl = readline.createInterface({input: inputStream})

rl.on('line', (line) ->
  freq += parseChange(line)
)

rl.on('close', () ->
  console.log(freq)
)

parseChange = (change) ->
  sign = 1
  num = 0
  if change[0] == '-'
    sign = -1
    change = change.slice(1)
  num = parseInt(change)
  return sign * num
