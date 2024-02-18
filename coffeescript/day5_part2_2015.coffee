fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  count = 0
  lines = data.split '\n'
  for line in lines
    if line.match(/(..).*\1/) and line.match(/(.).\1/)
      count++
  
  console.log count
