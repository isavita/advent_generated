
# Function to read the input from a file and parse it into an initial state.
readInput = (filename) ->
  lines = require('fs').readFileSync(filename, 'utf-8').trim().split('\n')
  initialState = {}
  for y in [0...lines.length]
    for x in [0...lines[y].length]
      if lines[y][x] is '#'
        initialState[[x, y, 0, 0].join(',')] = true  # Store active cubes as keys in a hash
  initialState

# Function to get the neighbors of a cube in 4D space.
getNeighbors = (x, y, z, w) ->
  neighbors = []
  for dx in [-1..1]
    for dy in [-1..1]
      for dz in [-1..1]
        for dw in [-1..1]
          if dx isnt 0 or dy isnt 0 or dz isnt 0 or dw isnt 0
            neighbors.push [x + dx, y + dy, z + dz, w + dw]
  neighbors

# Function to simulate one cycle of the Conway Cubes in 4D.
simulateCycle = (activeCubes) ->
  newActiveCubes = {}
  potentialCubes = {}

  # Identify all cubes that are either active or neighbors of active cubes
  for coordStr of activeCubes
      [x, y, z, w] = coordStr.split(',').map(Number)
      potentialCubes[coordStr] = true
      for neighbor in getNeighbors(x, y, z, w)
          potentialCubes[neighbor.join(',')] = true
  
  # Iterate and apply rules on potential Cubes
  for coordStr of potentialCubes
    [x, y, z, w] = coordStr.split(',').map(Number)
    neighborCount = 0
    for neighbor in getNeighbors(x, y, z, w)
      if activeCubes[neighbor.join(',')]
        neighborCount++

    if activeCubes[coordStr]
      if neighborCount is 2 or neighborCount is 3
        newActiveCubes[coordStr] = true
    else
      if neighborCount is 3
        newActiveCubes[coordStr] = true

  newActiveCubes

# Function to run the simulation for a given number of cycles.
runSimulation = (initialState, cycles) ->
  activeCubes = initialState
  for i in [1..cycles]
    activeCubes = simulateCycle(activeCubes)
  activeCubes

# Main execution
initialState = readInput('input.txt')
finalState = runSimulation(initialState, 6)
console.log Object.keys(finalState).length
