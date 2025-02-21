
fs = require 'fs'

# Function to calculate the distance traveled for a given holding time and total race time.
calculateDistance = (holdTime, totalTime) ->
  holdTime * (totalTime - holdTime)

# Function to find the number of ways to beat the record for a single race.
findWaysToWin = (time, recordDistance) ->
  waysToWin = 0
  for holdTime in [0..time]
    distance = calculateDistance(holdTime, time)
    if distance > recordDistance
      waysToWin++
  waysToWin

# Read the input from the file.
input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

# Parse the times and distances from the input.
times = input[0].split(':')[1].trim().split(/\s+/).map(Number)
distances = input[1].split(':')[1].trim().split(/\s+/).map(Number)

# Calculate the product of the number of ways to win for each race.
productOfWays = 1
for i in [0...times.length]
  productOfWays *= findWaysToWin(times[i], distances[i])

# Print the final result.
console.log productOfWays

# --- Part Two ---
#   As the race is about to start, you realize the piece of paper with race times and record distances you got earlier actually just has very bad kerning. There's really only one race - ignore the spaces between the numbers on each line.
#   How many ways can you beat the record in this one much longer race?

# Parse input for part two, joining the numbers without spaces.
timePart2 = parseInt(input[0].split(':')[1].replace(/\s+/g, ''), 10)
distancePart2 = parseInt(input[1].split(':')[1].replace(/\s+/g, ''), 10)
console.log findWaysToWin(timePart2, distancePart2)

# Optimized findWaysToWin using binary search.  Much faster for part 2.
findWaysToWinOptimized = (time, recordDistance) ->
    # Function to perform binary search
    binarySearch = (low, high, findLowerBound) ->
      while low <= high
          mid = Math.floor((low + high) / 2)
          distance = calculateDistance(mid, time)

          if distance > recordDistance
              if findLowerBound
                  high = mid - 1
              else
                  low = mid + 1
          else
              if findLowerBound
                  low = mid + 1
              else
                  high = mid - 1
      if findLowerBound then low else high

    #Find the lower bound
    lowerBound = binarySearch(0, Math.floor(time / 2), true)
    # Find the upper bound.  We can optimize the search range.
    upperBound = binarySearch(Math.floor(time / 2), time, false)

    upperBound - lowerBound + 1


console.log findWaysToWinOptimized(timePart2, distancePart2)

