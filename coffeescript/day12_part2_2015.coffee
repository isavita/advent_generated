fs = require 'fs'

# Function to recursively sum all numbers in a JSON object, ignoring objects with "red"
sumNumbersIgnoringRed = (obj) ->
  sum = 0
  if Array.isArray(obj)
    # If it's an array, simply iterate and sum
    for item in obj
      sum += sumNumbersIgnoringRed(item)
  else if typeof obj == 'object' and obj != null
    # If it's an object, check for "red" values before summing
    if not anyPropertyIsRed(obj)
      for _, value of obj
        sum += sumNumbersIgnoringRed(value)
  else if typeof obj == 'number'
    sum += obj
  sum

# Helper function to check if any property of an object is "red"
anyPropertyIsRed = (obj) ->
  for _, value of obj
    return true if value == 'red'
  false

# Read the JSON document from input.txt, parse it, and sum numbers according to Part Two's rule
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return

  json = JSON.parse(data)
  sum = sumNumbersIgnoringRed(json)
  console.log sum

