
fs = require 'fs'

sum = 0

readline = require('readline').createInterface({
  input: fs.createReadStream('input.txt'),
  console: false
})

readline.on('line', (line) ->
  if line.length > 0
    firstDigit = lastDigit = -1
    for char, index in line
      if /\d/.test(char)
        if firstDigit == -1
          firstDigit = parseInt(char)
        lastDigit = parseInt(char)
    if firstDigit != -1 and lastDigit != -1
      sum += firstDigit * 10 + lastDigit
)

readline.on('close', ->
  console.log sum
)
