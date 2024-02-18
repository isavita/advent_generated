fs = require 'fs'

abbaRegex = /([a-z])(?!\1)([a-z])\2\1/g
hypernetRegex = /\[([a-z]+)\]/g

supportsTLS = (ip) ->
  hasABBA = false
  invalidABBA = false

  # Check for ABBAs outside hypernet sequences
  while match = abbaRegex.exec(ip) 
    hasABBA = true

  # Check for ABBAs inside hypernet sequences 
  while match = hypernetRegex.exec(ip)
    hypernetSequence = match[1]
    if abbaRegex.test(hypernetSequence)
      invalidABBA = true

  hasABBA and not invalidABBA

# Read input from 'input.txt'
fs.readFile 'input.txt', 'utf-8', (err, data) ->
  if err then throw err

  count = 0
  for ip in data.split('\n')
    if supportsTLS(ip)
      count += 1

  console.log(count) 
