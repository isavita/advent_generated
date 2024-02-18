
fs = require 'fs'

maxCalories = 0
currentCalories = 0

input = fs.readFileSync('input.txt', 'utf8').split('\n')

for line in input
  if line == ''
    if currentCalories > maxCalories
      maxCalories = currentCalories
    currentCalories = 0
    continue
  currentCalories += parseInt(line)

if currentCalories > maxCalories
  maxCalories = currentCalories

console.log maxCalories
