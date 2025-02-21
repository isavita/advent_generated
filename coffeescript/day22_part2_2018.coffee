
fs = require 'fs'

# Helper function to calculate the geologic index
calculateGeologicIndex = (x, y, targetX, targetY, erosionLevels) ->
  if (x is 0 and y is 0) or (x is targetX and y is targetY)
    return 0
  else if y is 0
    return x * 16807
  else if x is 0
    return y * 48271
  else
    return erosionLevels[x-1][y] * erosionLevels[x][y-1]

# Helper function to calculate the erosion level
calculateErosionLevel = (geologicIndex, depth) ->
  (geologicIndex + depth) % 20183

# Helper function to determine region type
getRegionType = (erosionLevel) ->
  erosionLevel % 3

# Read input from file
input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
depth = parseInt(input[0].split(': ')[1])
[targetX, targetY] = input[1].split(': ')[1].split(',').map(Number)


# --- Part One ---
erosionLevels = []
for x in [0..targetX]
  erosionLevels[x] = []
  for y in [0..targetY]
    geologicIndex = calculateGeologicIndex(x, y, targetX, targetY, erosionLevels)
    erosionLevels[x][y] = calculateErosionLevel(geologicIndex, depth)

totalRiskLevel = 0
for x in [0..targetX]
  for y in [0..targetY]
    totalRiskLevel += getRegionType(erosionLevels[x][y])

console.log "Part 1: Total risk level:", totalRiskLevel


# --- Part Two ---

# Constants for tools and region types
ROCKY = 0
WET = 1
NARROW = 2
TORCH = 0
CLIMBING_GEAR = 1
NEITHER = 2

# Function to check if a tool is valid for a region type
isValidTool = (regionType, tool) ->
    switch regionType
        when ROCKY then tool isnt NEITHER
        when WET then tool isnt TORCH
        when NARROW then tool isnt CLIMBING_GEAR

# Expand the grid for pathfinding (add a buffer)
maxX = targetX + 50
maxY = targetY + 50

expandedErosionLevels = []
for x in [0..maxX]
    expandedErosionLevels[x] = []
    for y in [0..maxY]
        geologicIndex = calculateGeologicIndex(x, y, targetX, targetY, expandedErosionLevels)
        expandedErosionLevels[x][y] = calculateErosionLevel(geologicIndex, depth)


# Dijkstra's algorithm
findShortestPath = (startX, startY, targetX, targetY, erosionLevels) ->
    
    maxX = erosionLevels.length - 1
    maxY = erosionLevels[0].length - 1

    distances = {}
    visited = {}
    queue = []  # Use a simple array as a queue

    #Initialize distances and queue
    for x in [0..maxX]
        for y in [0..maxY]
            for tool in [TORCH, CLIMBING_GEAR, NEITHER]
                distances[[x,y,tool]] = Infinity
                visited[[x,y,tool]] = false
    
    distances[[startX, startY, TORCH]] = 0
    queue.push({x: startX, y: startY, tool: TORCH, dist: 0}) # Store dist

    while queue.length > 0
        # Find the node with the smallest distance (priority queue mimic)
        minIndex = 0
        for i in [1...queue.length]
            if queue[i].dist < queue[minIndex].dist
                minIndex = i

        current = queue.splice(minIndex, 1)[0]
        u = {x: current.x, y: current.y, tool: current.tool}
        
        if visited[[u.x, u.y, u.tool]]
            continue
        visited[[u.x, u.y, u.tool]] = true

        #Check if target is reached. We must have the torch equipped at the target
        if u.x is targetX and u.y is targetY and u.tool is TORCH
            return distances[[u.x, u.y, u.tool]]

        # Explore neighbors (up, down, left, right)
        neighbors = [[u.x - 1, u.y], [u.x + 1, u.y], [u.x, u.y - 1], [u.x, u.y + 1]]
        for [nx, ny] in neighbors
            if nx >= 0 and nx <= maxX and ny >= 0 and ny <= maxY
                currentRegionType = getRegionType(erosionLevels[u.x][u.y])
                nextRegionType = getRegionType(erosionLevels[nx][ny])
                
                # Move with the same tool if possible
                if isValidTool(nextRegionType, u.tool)
                    alt = distances[[u.x, u.y, u.tool]] + 1
                    if alt < distances[[nx, ny, u.tool]]
                        distances[[nx, ny, u.tool]] = alt
                        queue.push({x: nx, y: ny, tool: u.tool, dist: alt})

        # Explore changing tools
        for nextTool in [TORCH, CLIMBING_GEAR, NEITHER]
            if nextTool isnt u.tool
                currentRegionType = getRegionType(erosionLevels[u.x][u.y])
                if isValidTool(currentRegionType, nextTool)
                    alt = distances[[u.x, u.y, u.tool]] + 7
                    if alt < distances[[u.x, u.y, nextTool]]
                        distances[[u.x, u.y, nextTool]] = alt
                        queue.push({x: u.x, y: u.y, tool: nextTool, dist: alt})
                        

    return Infinity # No path found
    

shortestTime = findShortestPath(0, 0, targetX, targetY, expandedErosionLevels)
console.log "Part 2: Fewest number of minutes:", shortestTime
