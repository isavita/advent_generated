fs = require 'fs'

# Recursive function to find all combinations that sum to the target
# and track the number of containers used in each combination
findCombinations = (containers, target, index = 0, currentCombination = [], allCombinations = []) ->
  if target == 0
    # When a valid combination is found, add it to allCombinations
    allCombinations.push currentCombination.slice()
    return
  if target < 0 or index >= containers.length
    return

  # Include the current container and recurse
  currentCombination.push containers[index]
  findCombinations(containers, target - containers[index], index + 1, currentCombination, allCombinations)

  # Exclude the current container and recurse
  currentCombination.pop()
  findCombinations(containers, target, index + 1, currentCombination, allCombinations)

# Function to find the minimum number of containers used among all combinations
findMinContainerCombinations = (allCombinations) ->
  # Find the minimum number of containers used in any combination
  minContainers = Math.min.apply Math, allCombinations.map((combination) -> combination.length)
  # Filter combinations to those that use the minimum number of containers
  minContainerCombinations = allCombinations.filter((combination) -> combination.length == minContainers)
  minContainerCombinations.length

# Read container sizes from input.txt, find combinations, and determine the minimum container solution
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return

  containers = data.trim().split('\n').map (line) -> parseInt(line)
  allCombinations = []

  findCombinations(containers, 150, 0, [], allCombinations)

  # Find how many combinations use the minimum number of containers
  minContainerCombinationCount = findMinContainerCombinations(allCombinations)

  console.log minContainerCombinationCount
