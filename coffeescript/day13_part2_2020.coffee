fs = require 'fs'

# Read input from file
input = fs.readFileSync 'input.txt', 'utf8'
lines = input.split '\n'

# Part 1
timestamp = parseInt lines[0]
busIds = lines[1].split(',').filter (id) -> id isnt 'x'
busIds = busIds.map (id) -> parseInt id

earliestBus = null
earliestTime = Infinity
for busId in busIds
  time = Math.ceil(timestamp / busId) * busId
  if time < earliestTime
    earliestTime = time
    earliestBus = busId

console.log "Part 1:", earliestBus * (earliestTime - timestamp)

# Part 2
busIds = lines[1].split(',').map (id, i) -> [id, i]
busIds = busIds.filter ([id]) -> id isnt 'x'

timestamp = 0
step = 1
for [busId, offset] in busIds
  busId = parseInt busId
  while (timestamp + offset) % busId isnt 0
    timestamp += step
  step *= busId

console.log "Part 2:", timestamp