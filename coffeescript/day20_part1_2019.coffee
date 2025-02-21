
fs = require 'fs'

# Helper function to find all occurrences of a character in a string
findAll = (str, char) ->
  indices = []
  for i in [0...str.length]
    indices.push(i) if str[i] is char
  indices

# Reads the maze from the file
readMaze = (filename) ->
  fs.readFileSync(filename, 'utf8').split('\n')

# Finds the coordinates of portals and their connections
findPortals = (maze) ->
  portals = {}
  portalCoords = {}

  # Scan horizontally
  for y in [0...maze.length]
    row = maze[y]
    for x in [0...row.length - 1]
      if row[x]?.match(/[A-Z]/) and row[x+1]?.match(/[A-Z]/)
        portalName = row[x] + row[x+1]
        portalCoords[portalName] ?= []
        if x > 0 and row[x-1] is '.'
            portalCoords[portalName].push [x-1, y]
        else if x < row.length-2 and row[x+2] is '.'
            portalCoords[portalName].push [x+2, y]
  
  # Scan Vertically
  for x in [0...maze[0].length]
      for y in [0...maze.length-1]
          if maze[y][x]?.match(/[A-Z]/) and maze[y+1][x]?.match(/[A-Z]/)
              portalName = maze[y][x] + maze[y+1][x]
              portalCoords[portalName] ?= []
              if y > 0 and maze[y-1][x] is '.'
                  portalCoords[portalName].push [x, y - 1]
              else if y < maze.length - 2 and maze[y+2][x] is '.'
                  portalCoords[portalName].push [x, y+2]

  # Connect the portals
  for name, coords of portalCoords
    if coords.length is 2
        portals[coords[0]] = coords[1]
        portals[coords[1]] = coords[0]

  portalCoords

# Breadth-first search to find the shortest path
bfs = (maze, portals, start, end) ->
  queue = [[start, 0]]  # [position, distance]
  visited = {}
  visited[start] = true

  while queue.length > 0
    [current, dist] = queue.shift()
    [cx, cy] = current

    return dist if cx is end[0] and cy is end[1]

    # Explore adjacent cells
    for [dx, dy] in [[0, 1], [0, -1], [1, 0], [-1, 0]]
      nx = cx + dx
      ny = cy + dy
      nextPos = [nx, ny]
      if maze[ny]?[nx] is '.' and not visited[nextPos]
        visited[nextPos] = true
        queue.push([nextPos, dist + 1])

    # Check for portals
    if portals[current]
      portalDest = portals[current]
      if not visited[portalDest]
        visited[portalDest] = true
        queue.push([portalDest, dist + 1])
  -1 # No path found


# Main execution
maze = readMaze('input.txt')
portalCoords = findPortals(maze)
portals = {}

# Create a dictionary for portals that maps coordinates to their destination
for name, coords of portalCoords
    if coords.length is 2
        portals[coords[0].toString()] = coords[1]
        portals[coords[1].toString()] = coords[0]


start = portalCoords['AA'][0]
end = portalCoords['ZZ'][0]
# Run the BFS and print the result
distance = bfs(maze, portals, start, end)
console.log distance
