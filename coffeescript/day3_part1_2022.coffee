fs = require 'fs'

itemPriority = (item) ->
  if item >= 'a' and item <= 'z'
    item.charCodeAt(0) - 'a'.charCodeAt(0) + 1
  else
    item.charCodeAt(0) - 'A'.charCodeAt(0) + 27

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error opening file:", err
    return

  sum = 0
  lines = data.split '\n'
  for line in lines
    continue unless line.length > 0
    half = Math.floor line.length / 2
    firstCompartment = line[0...half]
    secondCompartment = line[half...]

    compartmentMap = {}
    for item in firstCompartment
      compartmentMap[item] = (compartmentMap[item] or 0) + 1

    for item in secondCompartment
      if compartmentMap[item]?
        sum += itemPriority item
        break

  console.log sum