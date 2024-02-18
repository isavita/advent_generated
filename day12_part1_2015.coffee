fs = require 'fs'

# Function to recursively sum all numbers in a JSON object
sumNumbers = (obj, filter) ->
  sum = 0
  # If the object is an array, iterate through elements
  if Array.isArray(obj)
    for item in obj
      sum += sumNumbers(item, filter)
  else if typeof obj == 'object' and obj != null
    # Placeholder for Part Two specific filtering logic, if any
    if filter and shouldFilterObject(obj)
      return 0
    for _, value of obj
      sum += sumNumbers(value, filter)
  else if typeof obj == 'number'
    sum += obj
  sum

# Placeholder for Part Two filtering function
shouldFilterObject = (obj) ->
  # Insert logic to determine if object should be filtered out
  false

# Read the JSON document from input.txt and calculate the sum of all numbers
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return

  json = JSON.parse(data)
  sum = sumNumbers(json, true) # Set true or false based on Part Two's requirements

  console.log sum
