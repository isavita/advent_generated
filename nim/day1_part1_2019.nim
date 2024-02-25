
import strutils, math

var fuelReqs: seq[int]

for line in lines("input.txt"):
  let mass = parseInt(line)
  let fuel = (mass / 3).int - 2
  fuelReqs.add(fuel)

echo sum(fuelReqs)
