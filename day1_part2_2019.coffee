
fs = require 'fs'

processLine = (line) ->
  n =parseInt(line.trim(), 10)
  if isNaN(n)
    console.log "Error parsing line"
    return 0
  return n

calcFuelMass = (mass) ->
  fuel = Math.floor(mass / 3) - 2
  if fuel <= 0
    return 0
  return fuel + calcFuelMass(fuel)

getTotal = (masses) ->
  total = 0
  for mass in masses
    total += calcFuelMass(mass)
  return total

inputFile = './input.txt'
fs.readFile inputFile, 'utf8', (err, data) ->
  if err
    console.log "Error reading file: #{err}"
    process.exit(1)
  masses = data.split('\n').map processLine
  total = getTotal(masses)
  console.log total
