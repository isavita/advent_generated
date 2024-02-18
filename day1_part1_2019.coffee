
masses = []
total = 0

processLine = (line) ->
  try
    m = parseInt(line.trim(), 10)
    masses.push(m)
  catch error
    console.log("Error parsing line")

getTotal = ->
  tempTotal = 0
  for i in [0...masses.length]
    tempTotal += Math.floor(masses[i]/3) - 2
  total = tempTotal

fs = require('fs')

lines = fs.readFileSync('input.txt').toString().split('\n').filter(Boolean)
for line in lines
  processLine(line)

getTotal()
console.log(total)
