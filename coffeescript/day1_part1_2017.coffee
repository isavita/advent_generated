
fs = require 'fs'

try
  data = fs.readFileSync('input.txt', 'utf8').trim()
  sum = 0
  for i in [0...data.length]
    j = (i + 1) % data.length
    if data[i] == data[j] then sum += parseInt(data[i])
  console.log sum
catch err
  console.error "File reading error: #{err}"
