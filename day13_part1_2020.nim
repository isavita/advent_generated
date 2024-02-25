
import strutils

var file = open("input.txt")
let earliestDeparture = file.readLine().parseInt()
let busIDs = file.readLine().split(',')

var earliestBusID = 0
var minWaitTime = earliestDeparture

for id in busIDs:
    if id == "x":
        continue
    let busID = parseInt(id)
    let waitTime = busID - (earliestDeparture mod busID)
    if waitTime < minWaitTime:
        minWaitTime = waitTime
        earliestBusID = busID

echo earliestBusID * minWaitTime
