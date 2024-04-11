class Coord:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Coord(self.x + other.x, self.y + other.y)

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        return hash((self.x, self.y))


class Grid:
    def __init__(self, width, height, data):
        self.width = width
        self.height = height
        self.data = data


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


class Edge:
    def __init__(self, start, end, weight):
        self.start = start
        self.end = end
        self.weight = weight


class Graph:
    def __init__(self):
        self.vertices = set()
        self.edges = {}


def is_in_bounds(grid, coord):
    return 0 <= coord.x < grid.width and 0 <= coord.y < grid.height


def parse_input(input):
    grid = Grid(len(input[0]), len(input), {})
    for y, line in enumerate(input):
        for x, char in enumerate(line):
            if char != Empty:
                grid.data[Coord(x, y)] = char
    return grid


def is_valid_neighbor(grid, coord, dir):
    if not is_in_bounds(grid, coord):
        return False
    if grid.data.get(coord, Empty) == Wall:
        return False
    return True


def neighbors4(grid, coord, is_valid_neighbor_func):
    directions = [North, South, West, East]
    valid_neighbors = []
    for dir in directions:
        neighbor = coord + dir
        if is_valid_neighbor_func(grid, neighbor, dir):
            valid_neighbors.append(neighbor)
    return valid_neighbors


def get_graph(grid, start, end, is_valid_neighbor_func):
    graph = Graph()
    graph.vertices.add(start)
    graph.vertices.add(end)

    for y in range(grid.height):
        for x in range(grid.width):
            coord = Coord(x, y)
            if coord not in grid.data:
                if len(neighbors4(grid, coord, is_valid_neighbor)) > 2:
                    graph.vertices.add(coord)

    for start in graph.vertices:
        edges = get_edges_bfs(grid, start, graph.vertices, is_valid_neighbor_func)
        graph.edges[start] = edges

    return graph


def get_edges_bfs(grid, start, vertices, is_valid_neighbor_func):
    frontier = [start]
    reached = {start}
    distances = {start: 0}
    edges = {}

    while frontier:
        current = frontier.pop(0)

        if current in vertices and current != start:
            edge = Edge(start, current, distances[current])
            edges[edge] = None
            continue

        for next in neighbors4(grid, current, is_valid_neighbor_func):
            if next not in reached:
                frontier.append(next)
                reached.add(next)
                distances[next] = distances[current] + 1

    return edges


def get_max_distance_dfs(grid, graph, current, end, seen):
    if current == end:
        return True, 0

    maxi = 0
    seen.add(current)
    for edge in graph.edges.get(current, {}):
        if edge.end not in seen:
            is_valid, dist = get_max_distance_dfs(grid, graph, edge.end, end, seen)
            if is_valid:
                maxi = max(maxi, dist + edge.weight)
    seen.remove(current)

    if maxi == 0:
        return False, 0
    return True, maxi


def solve(input):
    grid = parse_input(input)

    start = Coord(1, 0)
    end = Coord(grid.width - 2, grid.height - 1)

    graph = get_graph(grid, start, end, is_valid_neighbor)

    _, max_dist = get_max_distance_dfs(grid, graph, start, end, set())
    return max_dist


def read_file(file_name):
    with open(file_name, 'r') as file:
        return [line.strip() for line in file.readlines()]


def main():
    input = read_file("input.txt")
    print(solve(input))


if __name__ == "__main__":
    main()