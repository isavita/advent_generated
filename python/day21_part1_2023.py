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


class Grid:
    def __init__(self, width, height, data):
        self.width = width
        self.height = height
        self.data = data

    def is_in_bounds(self, coord):
        return 0 <= coord.x < self.width and 0 <= coord.y < self.height


def parse_input(input):
    height = len(input)
    width = len(input[0])
    data = {}
    for y, line in enumerate(input):
        for x, char in enumerate(line):
            if char != '.':
                data[Coord(x, y)] = char
    return Grid(width, height, data)


def find_start(grid):
    for coord, char in grid.data.items():
        if char == 'S':
            return coord
    raise ValueError("No start found.")


def neighbors4(grid, coord):
    directions = [Coord(0, -1), Coord(0, 1), Coord(1, 0), Coord(-1, 0)]
    valid_neighbors = []
    for direction in directions:
        neighbor = coord + direction
        if grid.is_in_bounds(neighbor) and grid.data.get(neighbor, '.') != '#':
            valid_neighbors.append(neighbor)
    return valid_neighbors


def breadth_first_search(grid, start, neighbor_func):
    frontier = [start]
    reached = {start}
    distances = {start: 0}

    while frontier:
        current = frontier.pop(0)
        for next in neighbor_func(grid, current):
            if next not in reached:
                frontier.append(next)
                reached.add(next)
                distances[next] = distances[current] + 1

    return distances


def solve(input, num_steps):
    grid = parse_input(input)
    start = find_start(grid)
    distances = breadth_first_search(grid, start, neighbors4)

    cnt = 0
    for dist in distances.values():
        if dist <= num_steps and dist % 2 == 0:
            cnt += 1
    return cnt


def read_file(file_name):
    with open(file_name, 'r') as file:
        return [line.strip() for line in file.readlines()]


def main():
    input = read_file("input.txt")
    print(solve(input, 64))


if __name__ == "__main__":
    main()