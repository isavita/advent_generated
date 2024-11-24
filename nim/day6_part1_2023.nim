
import strutils, sequtils

# Function to calculate the number of ways to beat the record for a single race
proc countWaysToBeatRecord(time, record: int): int =
  var ways = 0
  for holdTime in 1 ..< time:
    let travelTime = time - holdTime
    let distance = holdTime * travelTime
    if distance > record:
      ways += 1
  return ways

# Main procedure to read input and calculate the result
proc main() =
  # Read input from file
  let input = readFile("input.txt")
  let lines = input.splitLines()

  # Parse the time and distance values
  let times = lines[0].splitWhitespace()[1..^1].map(parseInt)
  let distances = lines[1].splitWhitespace()[1..^1].map(parseInt)

  # Calculate the number of ways to beat the record for each race
  var result = 1
  for i in 0 ..< times.len:
    result *= countWaysToBeatRecord(times[i], distances[i])

  # Print the final result
  echo result

# Run the main procedure
main()
