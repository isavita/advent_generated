fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "File reading error", err
    return

  lines = data.trim().split '\n'
  sum = 0

  for line in lines
    nums = (parseInt(num) for num in line.split(/\s+/))
    for i in [0...nums.length]
      for j in [0...nums.length]
        if i != j and nums[i] % nums[j] == 0
          sum += nums[i] / nums[j]

  console.log sum