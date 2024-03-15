class Coord:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def add(self, c2):
        return Coord(self.x + c2.x, self.y + c2.y)

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
        self.data = {Coord(x, y): char for y, line in enumerate(input_data) for x, char in enumerate(line) if char != '.'}

    def __str__(self):
        result = ""
        for y in range(self.height):
            for x in range(self.width):
                coord = Coord(x, y)
                result += self.data.get(coord, '.')
            result += "\n"
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

def cycle_rocks(grid):
    shift_rocks(grid, Coord(0, -1))
    shift_rocks(grid, Coord(-1, 0))
    shift_rocks(grid, Coord(0, 1))
    shift_rocks(grid, Coord(1, 0))

def calculate_grid_key(grid):
    key = 0
    for coord, char in grid.data.items():
        if char == 'O':
            key += coord.x + coord.y * grid.width
    return key

def calculate_load(grid):
    load = 0
    for coord, char in grid.data.items():
        if char == 'O':
            load += grid.height - coord.y
    return load

def solve(input_data):
    num_cycles = 1000000000
    grid = Grid(input_data)
    cache = {}

    for i in range(num_cycles):
        grid_key = calculate_grid_key(grid)
        if grid_key in cache:
            i_start_cycle = cache[grid_key]
            cycle_length = i - i_start_cycle
            remaining_cycles = (num_cycles - i_start_cycle) % cycle_length
            for _ in range(remaining_cycles):
                cycle_rocks(grid)
            return calculate_load(grid)
        cache[grid_key] = i
        cycle_rocks(grid)

    return calculate_load(grid)

def read_file(file_name):
    with open(file_name, 'r') as file:
        return [line.strip() for line in file.readlines()]

if __name__ == '__main__':
    input_data = read_file('input.txt')
    print(solve(input_data))