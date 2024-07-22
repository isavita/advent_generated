
import sequtils, strutils, os

# Function to calculate the total fuel cost for aligning to a specific position
proc calculateFuelCost(crabs: seq[int], target: int): int =
  result = 0
  for crab in crabs:
    result += abs(crab - target)

# Main procedure
proc main() =
  # Read input from file
  let input = readFile("input.txt")
  let crabs = input.split(',').mapIt(it.parseInt())

  # Determine the minimum and maximum positions
  let minPos = crabs.min()
  let maxPos = crabs.max()

  # Initialize variables to find the optimal position
  var minFuel = high(int)
  var bestPosition = -1

  # Check each position from min to max
  for pos in minPos..maxPos:
    let fuelCost = calculateFuelCost(crabs, pos)
    if fuelCost < minFuel:
      minFuel = fuelCost
      bestPosition = pos

  # Print the result
  echo "Best position: ", bestPosition
  echo "Minimum fuel cost: ", minFuel

# Execute the main procedure
main()
