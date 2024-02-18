
fs = require 'fs'

caloriesList = []
currentCalories = 0

input = fs.readFileSync('input.txt', 'utf8').split('\n')

for line in input
  if line == ''
    caloriesList.push currentCalories
    currentCalories = 0
    continue
  currentCalories += parseInt(line)

caloriesList.push currentCalories
caloriesList.sort (a, b) -> b - a
topThreeSum = caloriesList[0] + caloriesList[1] + caloriesList[2]
console.log topThreeSum
