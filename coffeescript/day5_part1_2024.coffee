
fs = require 'fs'

readInput = (filename) ->
  try
    data = fs.readFileSync(filename, 'utf8').trim().split('\n')
  catch error
    console.log "Error reading input:", error
    return [[], []]

  orderingRules = []
  updates = []
  isUpdateSection = false

  for line in data
    line = line.trim()
    if not line
      isUpdateSection = true
      continue

    if not isUpdateSection
      parts = line.split('|').map (p) -> p.trim()
      if parts.length is 2
        x = parseInt(parts[0], 10)
        y = parseInt(parts[1], 10)
        if not isNaN(x) and not isNaN(y)
          orderingRules.push [x, y]
    else
      nums = line.split(',').map (n) -> parseInt(n.trim(), 10)
      nums = nums.filter (n) -> not isNaN(n)
      if nums.length > 0
        updates.push nums

  [orderingRules, updates]

isCorrectlyOrdered = (update, rules) ->
  position = {}
  for page, idx in update
    position[page] = idx

  for rule in rules
    [x, y] = rule
    posX = position[x]
    posY = position[y]
    if posX? and posY? and posX >= posY
      return false
  true

[orderingRules, updates] = readInput('input.txt')

sum = 0
for update in updates
  if isCorrectlyOrdered(update, orderingRules)
    sum += update[Math.floor(update.length / 2)]

console.log sum
