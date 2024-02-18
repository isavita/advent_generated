fs = require 'fs'

# MFCSAM output as our criteria
criteria = {
  children: 3
  cats: 7
  samoyeds: 2
  pomeranians: 3
  akitas: 0
  vizslas: 0
  goldfish: 5
  trees: 3
  cars: 2
  perfumes: 1
}

# Function to parse Aunt Sue details and check against the criteria
matchesCriteria = (sueDetails) ->
  for own key, value of sueDetails
    return false if criteria[key] != value
  true

# Reading the input file and processing each Aunt Sue
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return

  sues = data.trim().split('\n')
  for sue in sues
    # Parse each line into an object of details
    sueDetails = {}
    [_, sueNumber, details] = sue.match /^Sue (\d+): (.+)$/
    for detail in details.split(', ')
      [key, value] = detail.split(': ')
      sueDetails[key] = parseInt(value)

    # Check if the current Aunt Sue matches the criteria
    if matchesCriteria(sueDetails)
      console.log sueNumber
      break

