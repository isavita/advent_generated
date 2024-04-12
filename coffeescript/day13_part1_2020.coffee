fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return

  [earliestDeparture, busIDs] = data.trim().split '\n'
  earliestDeparture = parseInt earliestDeparture
  busIDs = busIDs.split ','

  earliestBusID = 0
  minWaitTime = Infinity

  for id in busIDs
    continue if id == 'x'
    busID = parseInt id
    waitTime = busID - (earliestDeparture % busID)
    if waitTime < minWaitTime
      minWaitTime = waitTime
      earliestBusID = busID

  console.log earliestBusID * minWaitTime