
fs = require('fs')

try
  data = fs.readFileSync('input.txt', 'utf8')
catch err
  console.error("Error reading file: #{err}")
  process.exit(1)

frequencyChanges = data.trim().split("\n")
frequencies = {}
currentFrequency = 0
frequencies[currentFrequency] = true

while true
  for change in frequencyChanges
    frequencyDelta = parseInt(change, 10)
    if isNaN(frequencyDelta)
      console.error("Error converting string to int: #{change}")
      process.exit(1)
    currentFrequency += frequencyDelta
    if frequencies[currentFrequency]
      console.log(currentFrequency)
      return
    frequencies[currentFrequency] = true
