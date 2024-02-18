
fs = require 'fs'

prev = 0
count = 0

readline = require('readline')

try
  inputStream = fs.createReadStream('input.txt')
catch error
  console.error("Error opening input file:", error.message)
  process.exit(1)

rl = readline.createInterface({
  input: inputStream,
  terminal: false
})

rl.on('line', (line) ->
  current = parseInt(line, 10)
  if prev != 0 and current > prev
    count++
  prev = current
)

rl.on('close', ->
  console.log count
)
