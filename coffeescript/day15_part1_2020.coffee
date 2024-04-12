fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return
  
  startingNumbers = data.trim().split ','
  
  lastSpoken = {}
  lastNumber = undefined
  nextNumber = undefined
  
  for turn in [1..2020]
    if turn - 1 < startingNumbers.length
      lastNumber = parseInt startingNumbers[turn - 1]
      lastSpoken[lastNumber] = turn
      continue
    
    if lastSpoken[lastNumber]? and lastSpoken[lastNumber] != turn - 1
      nextNumber = turn - 1 - lastSpoken[lastNumber]
    else
      nextNumber = 0
    
    lastSpoken[lastNumber] = turn - 1
    lastNumber = nextNumber
  
  console.log lastNumber