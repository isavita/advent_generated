fs = require 'fs'

countCanContain = (target, contains) ->
  seen = {}
  dfs = (bag) ->
    # Ensure that we have a valid array to iterate over
    outerBags = contains[bag] or []
    for outer in outerBags
      unless seen[outer]
        seen[outer] = true
        dfs outer
  dfs target
  return Object.keys(seen).length

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error opening file:", err
    return

  contains = {}
  lines = data.split '\n'
  for line in lines
    continue unless line
    [container, content] = line.split ' bags contain '
    continue if content == 'no other bags.'
    containedBags = content.split ', '
    for bag in containedBags
      # Extract the bag name correctly, accounting for number and bag description
      bagDescription = bag.match /(\d+) ([a-z]+ [a-z]+)/
      if bagDescription?
        bagName = bagDescription[2]
        contains[bagName] = (contains[bagName] or []).concat container

  # Initialize empty arrays for any bags not already in the dictionary
  for line in lines
    continue unless line
    [container, content] = line.split ' bags contain '
    contains[container] = contains[container] or []

  count = countCanContain 'shiny gold', contains
  console.log count