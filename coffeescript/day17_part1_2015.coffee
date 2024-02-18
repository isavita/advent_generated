
# Read input from file "input.txt"
fs = require 'fs'
input = fs.readFileSync('input.txt', 'utf8')
containers = (parseInt(line) for line in input.split('\n'))

# Count combinations of containers that sum up to the target amount
countCombinations = (containers, target, index) ->
  if target == 0 then 1
  else if target < 0 or index >= containers.length then 0
  else countCombinations(containers, target - containers[index], index + 1) +
       countCombinations(containers, target, index + 1)

console.log countCombinations(containers, 150, 0)
