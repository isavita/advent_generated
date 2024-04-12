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

  lines = data.split '\n'
  sum = 0
  groupLineCounter = 0
  groupItems = []

  for line in lines
    if line.trim() == ''
      continue

    itemsMap = {}
    for item in line
      itemsMap[item] = (itemsMap[item] or 0) + 1

    groupItems[groupLineCounter] = itemsMap
    groupLineCounter++

    if groupLineCounter == 3
      commonItems = {}
      for item of groupItems[0]
        if groupItems[1][item]? and groupItems[2][item]?
          commonItems[item] = true

      for item of commonItems
        sum += itemPriority item
        break # Since we need only one common item per group

      groupLineCounter = 0

  console.log sum