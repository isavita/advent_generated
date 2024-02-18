# Path/filename: /mnt/data/solveSingleNight.coffee

fs = require 'fs'

# Function to generate all permutations of an array
permute = (arr) ->
  if arr.length == 0
    return [[]]
  permArr = []
  for i in [0...arr.length]
    rest = arr.slice(0, i).concat(arr.slice(i+1))
    for subPerm in permute(rest)
      permArr.push([arr[i]].concat(subPerm))
  permArr

# Function to calculate the total distance of a route
calculateDistance = (route, distances) ->
  totalDistance = 0
  for i in [0...route.length-1]
    totalDistance += distances[route[i]][route[i+1]] if i < route.length - 1
  totalDistance

# Parse input and build distances map
fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  lines = data.trim().split '\n'
  distances = {}
  locations = new Set()
  for line in lines
    [start, , end, , distance] = line.split ' '
    distances[start] = distances[start] or {}
    distances[end] = distances[end] or {}
    distances[start][end] = parseInt(distance, 10)
    distances[end][start] = parseInt(distance, 10)
    locations.add(start)
    locations.add(end)
  
  # Generate all permutations of locations
  routes = permute(Array.from(locations))
  
  # Calculate the distance of each route to find the shortest one
  shortestDistance = Infinity
  for route in routes
    distance = calculateDistance(route, distances)
    shortestDistance = Math.min(shortestDistance, distance)
  
  console.log shortestDistance
