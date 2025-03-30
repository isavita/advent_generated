
import strutils, tables, sets, deques, hashes, os

type
  Coord = object
    x, y: int

proc `+`(a, b: Coord): Coord =
  Coord(x: a.x + b.x, y: a.y + b.y)

proc hash(c: Coord): Hash =
  var h: Hash = 0
  h = h !& hash(c.x)
  h = h !& hash(c.y)
  return h

const
  North = Coord(x: 0, y: -1)
  South = Coord(x: 0, y: 1)
  West = Coord(x: -1, y: 0)
  East = Coord(x: 1, y: 0)

  Empty = '.'
  Wall = '#'
  NorthSlopes = '^'
  SouthSlopes = 'v'
  WestSlopes = '<'
  EastSlopes = '>'

let SlopeToDir = {
  NorthSlopes: North,
  SouthSlopes: South,
  WestSlopes: West,
  EastSlopes: East
}.toTable

type
  Grid = object
    width, height: int
    data: Table[Coord, char]

  Edge = tuple[start, dest: Coord, dist: int]

  Graph = object
    vertices: HashSet[Coord]
    edges: Table[Coord, HashSet[Edge]] # Maps start_node -> set of edges starting there

  IsValidNeighborFunc = proc (grid: Grid, coord, dir: Coord): bool {.closure.}


proc isInBounds(grid: Grid, coord: Coord): bool =
  coord.x >= 0 and coord.x < grid.width and coord.y >= 0 and coord.y < grid.height

proc parseInput(input: seq[string]): Grid =
  result.width = input[0].len
  result.height = input.len
  result.data = initTable[Coord, char]()

  for y, line in input:
    for x, char in line:
      if char != Empty:
        result.data[Coord(x: x, y: y)] = char

proc isValidNeighborNoSlopes(grid: Grid, coord, dir: Coord): bool =
  if not grid.isInBounds(coord):
    return false
  if coord in grid.data and grid.data[coord] == Wall:
    return false
  return true

proc isValidNeighborWithSlopes(grid: Grid, coord, dir: Coord): bool =
  if not grid.isInBounds(coord):
    return false
  if coord notin grid.data: # Empty space
    return true
  let cell = grid.data[coord]
  if cell == Wall:
    return false
  # If it's a slope, the direction we *entered* from must match the slope's direction
  # Example: If cell is '>', we must have come from the West (dir = East)
  if cell in SlopeToDir:
     return SlopeToDir[cell] == dir
  # Should not happen based on input, but handle defensively
  return true # Treat other potential chars as empty


proc neighbors4(grid: Grid, coord: Coord, isValidNeighborFunc: IsValidNeighborFunc): seq[Coord] =
  const directions = [North, South, West, East]
  for dir in directions:
    let neighbor = coord + dir
    if isValidNeighborFunc(grid, neighbor, dir):
      result.add(neighbor)

proc getEdgesBFS(grid: Grid, startNode: Coord, vertices: HashSet[Coord], isValidNeighborFunc: IsValidNeighborFunc): HashSet[Edge] =
  var frontier = initDeque[Coord]()
  frontier.addLast(startNode)
  var reached = initHashSet[Coord]()
  reached.incl(startNode)
  var distances = initTable[Coord, int]()
  distances[startNode] = 0
  result = initHashSet[Edge]()

  while frontier.len > 0:
    let current = frontier.popFirst()

    if current in vertices and current != startNode:
      # Found a path to another vertex node
      result.incl((start: startNode, dest: current, dist: distances[current]))
      # Stop exploring further from this path, as we only want direct edges between vertices
      continue

    # Use the *same* IsValidNeighborFunc used to build the graph edges
    # Note: The python version uses the *basic* neighbor check inside BFS,
    # which seems wrong for finding edges based on slopes. Let's use the
    # passed function. Reverting to basic if needed:
    # let validNeighbors = neighbors4(grid, current, isValidNeighborNoSlopes)
    let validNeighbors = neighbors4(grid, current, isValidNeighborFunc)

    for nextNode in validNeighbors:
      if nextNode notin reached:
        frontier.addLast(nextNode)
        reached.incl(nextNode)
        distances[nextNode] = distances[current] + 1


proc getGraph(grid: Grid, start, stop: Coord, isValidNeighborFunc: IsValidNeighborFunc): Graph =
  result.vertices = initHashSet[Coord]()
  result.vertices.incl(start)
  result.vertices.incl(stop)
  result.edges = initTable[Coord, HashSet[Edge]]()

  # Identify junction points (vertices) - empty cells with more than 2 non-wall neighbors
  for y in 0..<grid.height:
    for x in 0..<grid.width:
      let coord = Coord(x: x, y: y)
      if coord notin grid.data: # It's an empty path cell
        # Use the basic neighbor check (no slopes) to find potential junctions
        if neighbors4(grid, coord, isValidNeighborNoSlopes).len > 2:
          result.vertices.incl(coord)

  # Find edges between vertices using BFS
  for startNode in result.vertices:
    result.edges[startNode] = getEdgesBFS(grid, startNode, result.vertices, isValidNeighborFunc)


proc getMaxDistanceDFS(graph: Graph, current, stop: Coord, seen: var HashSet[Coord]): (bool, int) =
  if current == stop:
    return (true, 0)

  var maxDist = -1 # Use -1 to indicate no path found yet
  seen.incl(current)

  if current in graph.edges: # Check if the current node has outgoing edges
    for edge in graph.edges[current]:
      if edge.dest notin seen:
        let (isValid, dist) = getMaxDistanceDFS(graph, edge.dest, stop, seen)
        if isValid:
           let totalDist = dist + edge.dist
           if maxDist == -1 or totalDist > maxDist:
             maxDist = totalDist

  seen.excl(current) # Backtrack

  if maxDist == -1:
    return (false, 0) # No path found from this node
  else:
    return (true, maxDist)


proc solve(input: seq[string]): int =
  let grid = parseInput(input)
  let start = Coord(x: 1, y: 0)
  let stop = Coord(x: grid.width - 2, y: grid.height - 1)

  # Part 1: Use slopes to define valid paths for graph edges
  let graph = getGraph(grid, start, stop, isValidNeighborWithSlopes)

  var seen = initHashSet[Coord]()
  let (found, maxDist) = getMaxDistanceDFS(graph, start, stop, seen)
  if found:
    return maxDist
  else:
    return 0 # Or raise an error, path should be found

proc main() =
  let inputFile = "input.txt"
  if not fileExists(inputFile):
    echo "Error: input.txt not found"
    quit(1)
  
  let input = readFile(inputFile).strip().splitLines()
  echo solve(input)

main()
