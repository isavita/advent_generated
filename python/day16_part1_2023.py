class Coord:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        return hash((self.x, self.y))

    def add(self, other):
        return Coord(self.x + other.x, self.y + other.y)

    def rotate90(self):
        return Coord(self.y, -self.x)

    def rotate_neg90(self):
        return Coord(-self.y, self.x)

    def is_in_bounds(self, grid):
        return 0 <= self.x < grid.width and 0 <= self.y < grid.height


class Beam:
    def __init__(self, origin, direction):
        self.origin = origin
        self.dir = direction

    def __eq__(self, other):
        return self.origin == other.origin and self.dir == other.dir

    def __hash__(self):
        return hash((self.origin, self.dir))


class Grid:
    def __init__(self, width, height, data=None):
        self.width = width
        self.height = height
        self.data = data if data is not None else {}

    def __getitem__(self, coord):
        return self.data.get(coord, '.')


EMPTY = '.'
ASCENDING_MIRROR = '/'
DESCENDING_MIRROR = '\\'
VERTICAL_SPLITTER = '|'
HORIZONTAL_SPLITTER = '-'

NORTH = Coord(0, -1)
WEST = Coord(-1, 0)
SOUTH = Coord(0, 1)
EAST = Coord(1, 0)


def build_grid(input_lines):
    height = len(input_lines)
    width = len(input_lines[0])
    data = {}

    for y, line in enumerate(input_lines):
        for x, char in enumerate(line):
            if char != EMPTY:
                data[Coord(x, y)] = char

    return Grid(width, height, data)


def next_beam(grid, beam):
    beams = []
    char = grid[beam.origin]

    if char == EMPTY:
        new_beam = Beam(beam.origin.add(beam.dir), beam.dir)
        beams.append(new_beam)
    else:
        if char == ASCENDING_MIRROR:
            new_dir = beam.dir.rotate_neg90() if beam.dir in [NORTH, SOUTH] else beam.dir.rotate90()
            new_beam = Beam(beam.origin.add(new_dir), new_dir)
            beams.append(new_beam)
        elif char == DESCENDING_MIRROR:
            new_dir = beam.dir.rotate90() if beam.dir in [NORTH, SOUTH] else beam.dir.rotate_neg90()
            new_beam = Beam(beam.origin.add(new_dir), new_dir)
            beams.append(new_beam)
        elif char == VERTICAL_SPLITTER and beam.dir in [EAST, WEST]:
            new_dir1 = beam.dir.rotate90()
            new_dir2 = beam.dir.rotate_neg90()
            beams.append(Beam(beam.origin.add(new_dir1), new_dir1))
            beams.append(Beam(beam.origin.add(new_dir2), new_dir2))
        elif char == HORIZONTAL_SPLITTER and beam.dir in [NORTH, SOUTH]:
            new_dir1 = beam.dir.rotate90()
            new_dir2 = beam.dir.rotate_neg90()
            beams.append(Beam(beam.origin.add(new_dir1), new_dir1))
            beams.append(Beam(beam.origin.add(new_dir2), new_dir2))
        else:
            new_beam = Beam(beam.origin.add(beam.dir), beam.dir)
            beams.append(new_beam)

    return beams


def calculate_propagation(grid, start):
    already_seen = set()
    to_explore = [start]

    while to_explore:
        beam = to_explore.pop(0)

        if beam.origin.is_in_bounds(grid) and beam not in already_seen:
            already_seen.add(beam)
            to_explore.extend(next_beam(grid, beam))

    return already_seen


def calculate_energization(already_seen):
    already_energized = set()

    for beam in already_seen:
        already_energized.add(beam.origin)

    return already_energized


def solve(input_lines):
    grid = build_grid(input_lines)
    start = Beam(Coord(0, 0), EAST)

    already_seen = calculate_propagation(grid, start)
    already_energized = calculate_energization(already_seen)

    return len(already_energized)


def read_file(file_name):
    with open(file_name, 'r') as file:
        return [line.strip() for line in file.readlines()]


def main():
    input_lines = read_file("input.txt")
    print(solve(input_lines))


if __name__ == "__main__":
    main()