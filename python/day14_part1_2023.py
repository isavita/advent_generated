class Coord:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def add(self, other):
        return Coord(self.x + other.x, self.y + other.y)

    def is_in_bounds(self, grid):
        return 0 <= self.x < grid.width and 0 <= self.y < grid.height

    def __hash__(self):
        return hash((self.x, self.y))

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

class Grid:
    def __init__(self, input_data):
        self.width = len(input_data[0])
        self.height = len(input_data)
        self.data = {Coord(x, y): cell for y, line in enumerate(input_data) for x, cell in enumerate(line) if cell != '.'}

    def __str__(self):
        result = ''
        for y in range(self.height):
            for x in range(self.width):
                coord = Coord(x, y)
                result += self.data.get(coord, '.')
            result += '\n'
        return result

def shift_single_rock(grid, coord, direction):
    if coord in grid.data and grid.data[coord] == 'O':
        current = coord
        before = coord.add(direction)
        while before.is_in_bounds(grid) and before not in grid.data:
            grid.data[before] = 'O'
            del grid.data[current]
            current = before
            before = before.add(direction)

def shift_rocks(grid, direction):
    if direction in (Coord(0, -1), Coord(-1, 0)):
        for x in range(grid.width):
            for y in range(grid.height):
                shift_single_rock(grid, Coord(x, y), direction)
    else:
        for x in range(grid.width - 1, -1, -1):
            for y in range(grid.height - 1, -1, -1):
                shift_single_rock(grid, Coord(x, y), direction)

def calculate_load(grid):
    load = 0
    for x in range(grid.width):
        for y in range(grid.height):
            if Coord(x, y) in grid.data and grid.data[Coord(x, y)] == 'O':
                load += grid.height - y
    return load

def solve(input_data):
    grid = Grid(input_data)
    shift_rocks(grid, Coord(0, -1))
    return calculate_load(grid)

with open('input.txt', 'r') as f:
    input_data = [line.strip() for line in f.readlines()]

print(solve(input_data))