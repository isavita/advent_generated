fs = require 'fs'

# Adjusted MFCSAM output as our criteria, with special rules for part two
criteria = {
  children: {value: 3, comparison: 'equals'}
  cats: {value: 7, comparison: 'greater'}
  samoyeds: {value: 2, comparison: 'equals'}
  pomeranians: {value: 3, comparison: 'fewer'}
  akitas: {value: 0, comparison: 'equals'}
  vizslas: {value: 0, comparison: 'equals'}
  goldfish: {value: 5, comparison: 'fewer'}
  trees: {value: 3, comparison: 'greater'}
  cars: {value: 2, comparison: 'equals'}
  perfumes: {value: 1, comparison: 'equals'}
}

# Function to compare Aunt Sue details against adjusted criteria
matchesCriteriaPartTwo = (sueDetails) ->
  for own key, {value, comparison} of criteria
    continue unless sueDetails[key]?
    switch comparison
      when 'equals' then return false unless sueDetails[key] == value
      when 'greater' then return false unless sueDetails[key] > value
      when 'fewer' then return false unless sueDetails[key] < value
  true

# Reading the input file and processing each Aunt Sue for part two
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

    # Check if the current Aunt Sue matches the adjusted criteria for part two
    if matchesCriteriaPartTwo(sueDetails)
      console.log "Under the new criteria, Aunt Sue #{sueNumber} is the real sender."
      break

