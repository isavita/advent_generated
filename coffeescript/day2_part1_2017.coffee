fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "File reading error", err
    return

  lines = data.trim().split '\n'
  checksum = 0

  for line in lines
    nums = line.split(/\s+/).map (num) -> parseInt num, 10
    minVal = Math.min nums...
    maxVal = Math.max nums...
    checksum += (maxVal - minVal)

  console.log checksum