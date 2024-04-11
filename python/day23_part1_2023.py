from collections import deque, defaultdict

class Coord:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Coord(self.x + other.x, self.y + other.y)

    def __hash__(self):
        return hash((self.x, self.y))

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

North = Coord(0, -1)
South = Coord(0, 1)
West = Coord(-1, 0)
East = Coord(1, 0)

Empty = '.'
Wall = '#'
NorthSlopes = '^'
SouthSlopes = 'v'
WestSlopes = '<'
EastSlopes = '>'

SlopeToDir = {
    NorthSlopes: North,
    SouthSlopes: South,
    WestSlopes: West,
    EastSlopes: East,
}

def isInBounds(grid, coord):
    return 0 <= coord.x < grid['width'] and 0 <= coord.y < grid['height']

def parseInput(input):
    grid = {
        'width': len(input[0]),
        'height': len(input),
        'data': {}
    }

    for y, line in enumerate(input):
        for x, char in enumerate(line):
            if char != Empty:
                grid['data'][Coord(x, y)] = char

    return grid

def isValidNeighbor(grid, coord, dir):
    if not isInBounds(grid, coord):
        return False
    if coord in grid['data'] and grid['data'][coord] == Wall:
        return False
    return True

def isValidNeighborWithSlopes(grid, coord, dir):
    if not isInBounds(grid, coord):
        return False
    if coord not in grid['data']:
        return True
    if grid['data'][coord] == Wall:
        return False
    return SlopeToDir[grid['data'][coord]] == dir

def neighbors4(grid, coord, isValidNeighborFunc):
    directions = [North, South, West, East]
    validNeighbors = []

    for dir in directions:
        neighbor = coord + dir
        if isValidNeighborFunc(grid, neighbor, dir):
            validNeighbors.append(neighbor)

    return validNeighbors

def getGraph(grid, start, end, isValidNeighborFunc):
    graph = {
        'vertices': {start, end},
        'edges': defaultdict(dict)
    }

    for y in range(grid['height']):
        for x in range(grid['width']):
            coord = Coord(x, y)
            if coord not in grid['data']:
                if len(neighbors4(grid, coord, isValidNeighbor)) > 2:
                    graph['vertices'].add(coord)

    for start in graph['vertices']:
        edges = getEdgesBFS(grid, start, graph['vertices'], isValidNeighborFunc)
        graph['edges'][start] = edges

    return graph

def getEdgesBFS(grid, start, vertices, isValidNeighborFunc):
    frontier = deque([start])
    reached = {start}
    distances = {start: 0}
    edges = {}

    while frontier:
        current = frontier.popleft()

        if current in vertices and current != start:
            edge = (start, current, distances[current])
            edges[edge] = None
            continue

        for next in neighbors4(grid, current, isValidNeighborFunc):
            if next not in reached:
                frontier.append(next)
                reached.add(next)
                distances[next] = distances[current] + 1

    return edges

def getMaxDistanceDFS(grid, graph, current, end, seen):
    if current == end:
        return True, 0

    maxi = 0
    seen.add(current)
    for edge in graph['edges'][current]:
        if edge[1] not in seen:
            isValid, dist = getMaxDistanceDFS(grid, graph, edge[1], end, seen)
            if isValid:
                maxi = max(maxi, dist + edge[2])
    seen.remove(current)

    if maxi == 0:
        return False, 0
    return True, maxi

def solve(input):
    grid = parseInput(input)

    start = Coord(1, 0)
    end = Coord(grid['width'] - 2, grid['height'] - 1)

    graph = getGraph(grid, start, end, isValidNeighborWithSlopes)

    _, maxDist = getMaxDistanceDFS(grid, graph, start, end, set())
    return maxDist

with open('input.txt', 'r') as file:
    input = file.read().strip().split('\n')
    print(solve(input))