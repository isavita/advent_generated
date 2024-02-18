
fs = require 'fs'

try
  data = fs.readFileSync('input.txt', 'utf8').trim()
  halfway = Math.floor(data.length / 2)
  sum = 0

  for i in [0...data.length]
    next = (i + halfway) % data.length
    if data[i] == data[next]
      sum += parseInt(data[i], 10)

  console.log(sum)
catch err
  console.error("File reading error: #{err.message}")
