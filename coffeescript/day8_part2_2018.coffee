fs = require 'fs'

# Read input from the file
readInput = (filename) ->
  data = fs.readFileSync(filename, 'utf8').trim()
  numbers = (parseInt(num) for num in data.split(/\s+/))
  numbers

# Parse the tree and calculate the value of the root node
parseTree = (data, index) ->
  childCount = data[index]
  metaCount = data[index + 1]
  index += 2
  
  childValues = []
  for i in [0...childCount]
    result = parseTree(data, index)
    childValue = result[0]
    index = result[1]
    childValues.push childValue

  value = 0
  if childCount == 0
    for i in [0...metaCount]
      value += data[index + i]
  else
    for i in [0...metaCount]
      metadata = data[index + i]
      if metadata <= childCount and metadata > 0
        value += childValues[metadata - 1]

  index += metaCount
  [value, index]

# Main execution
numbers = readInput("input.txt")
result = parseTree(numbers, 0)
value = result[0]
console.log value