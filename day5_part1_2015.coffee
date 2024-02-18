fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  count = 0
  lines = data.split '\n'
  for line in lines
    if line.match(/[aeiou].*[aeiou].*[aeiou]/) and line.match(/(.)\1/) and not line.match(/ab|cd|pq|xy/)
      count++
  
  console.log count
