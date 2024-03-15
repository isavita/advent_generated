from dataclasses import dataclass, field
from typing import Dict, Set

@dataclass(frozen=True)
class Coord:
    x: int
    y: int

    def add(self, other):
        return Coord(self.x + other.x, self.y + other.y)

    def subtract(self, other):
        return Coord(self.x - other.x, self.y - other.y)

    def opposite(self):
        return Coord(-self.x, -self.y)

    def __hash__(self):
        return hash((self.x, self.y))

    def __eq__(self, other):
        if isinstance(other, Coord):
            return self.x == other.x and self.y == other.y
        return False

Tile = str
Pipe = Dict[Coord, Set[Coord]]

Undefined = Coord(0, 0)
Top = Coord(0, -1)
Right = Coord(1, 0)
Bottom = Coord(0, 1)
Left = Coord(-1, 0)

Empty = '.'
Start = 'S'
Vertical = '|'
Horizontal = '-'
TopLeftCorner = 'J'
TopRightCorner = 'L'
BottomLeftCorner = '7'
BottomRightCorner = 'F'
Enclosed = 'X'

VerticalPipe = {Top: set(), Bottom: set()}
HorizontalPipe = {Left: set(), Right: set()}
TopLeftCornerPipe = {Top: set(), Left: set()}
TopRightCornerPipe = {Top: set(), Right: set()}
BottomLeftCornerPipe = {Bottom: set(), Left: set()}
BottomRightCornerPipe = {Bottom: set(), Right: set()}

TileToPipe = {
    Vertical: VerticalPipe,
    Horizontal: HorizontalPipe,
    TopLeftCorner: TopLeftCornerPipe,
    TopRightCorner: TopRightCornerPipe,
    BottomLeftCorner: BottomLeftCornerPipe,
    BottomRightCorner: BottomRightCornerPipe,
}

def get_pipe_from_tile(tile: Tile) -> Pipe:
    if tile in TileToPipe:
        return TileToPipe[tile]
    return {}

def get_tile_from_pipe(pipe: Pipe) -> Tile:
    for tile, associated_pipe in TileToPipe.items():
        if is_equal_pipe(pipe, associated_pipe):
            return tile
    return Empty

def is_equal_pipe(pipe1: Pipe, pipe2: Pipe) -> bool:
    if len(pipe1) != len(pipe2):
        return False
    for dir in pipe1:
        if dir not in pipe2:
            return False
    return True

def build_grid(input_lines: list[str]) -> Dict[Coord, Tile]:
    grid = {}
    for y, line in enumerate(input_lines):
        for x, char in enumerate(line):
            if char != Empty:
                grid[Coord(x, y)] = char
    return grid

def find_start(grid: Dict[Coord, Tile]) -> Coord:
    for coord, value in grid.items():
        if value == Start:
            return coord
    return Coord(0, 0)

def get_pipe_from_neighbors(coord: Coord, grid: Dict[Coord, Tile]) -> Pipe:
    pipe = {}
    possible_neighbors = {
        Top: coord.add(Top),
        Right: coord.add(Right),
        Bottom: coord.add(Bottom),
        Left: coord.add(Left),
    }
    for dir, neighbor_coord in possible_neighbors.items():
        if neighbor_coord in grid:
            neighbor_pipe = get_pipe_from_tile(grid[neighbor_coord])
            if dir.opposite() in neighbor_pipe:
                pipe[dir] = set()
    return pipe

def path_finding(start: Coord, grid: Dict[Coord, Tile]) -> list[Coord]:
    path = [start]
    start_pipe = get_pipe_from_neighbors(start, grid)
    previous_dir = None
    current = None
    for dir in start_pipe:
        previous_dir = dir
        current = start.add(dir)

    while current != start:
        path.append(current)
        current_pipe = get_pipe_from_tile(grid[current])
        for dir in current_pipe:
            if dir != previous_dir.opposite():
                previous_dir = dir
                current = current.add(dir)
                break

    return path

def get_path_grid(grid: Dict[Coord, Tile], path: list[Coord], empty: Tile) -> Dict[Coord, Tile]:
    new_grid = {coord: grid[coord] for coord in path}
    start = path[0]
    new_grid[start] = get_tile_from_pipe(get_pipe_from_neighbors(start, grid))
    return new_grid

def is_inside(coord: Coord, grid: Dict[Coord, Tile], empty: Tile) -> bool:
    if coord in grid:
        return False
    start_pipe = empty
    num_pipe_on_left = 0
    for x in range(coord.x):
        c = Coord(x, coord.y)
        if c in grid:
            v = grid[c]
            if v == Vertical:
                num_pipe_on_left += 1
            elif v == TopRightCorner:
                start_pipe = TopRightCorner
            elif v == BottomRightCorner:
                start_pipe = BottomRightCorner
            elif v == TopLeftCorner:
                if start_pipe == BottomRightCorner:
                    start_pipe = empty
                    num_pipe_on_left += 1
                elif v == TopRightCorner:
                    start_pipe = Empty
            elif v == BottomLeftCorner:
                if start_pipe == TopRightCorner:
                    start_pipe = Empty
                    num_pipe_on_left += 1
                elif start_pipe == BottomRightCorner:
                    start_pipe = Empty
    return num_pipe_on_left % 2 == 1

def solve(input_lines: list[str]) -> int:
    grid = build_grid(input_lines)
    start = find_start(grid)
    path = path_finding(start, grid)
    path_grid = get_path_grid(grid, path, Empty)

    count = 0
    for y in range(len(input_lines)):
        for x in range(len(input_lines[0])):
            c = Coord(x, y)
            if is_inside(c, path_grid, Empty):
                count += 1
    return count

def read_file(file_name: str) -> list[str]:
    with open(file_name, 'r') as file:
        return [line.strip() for line in file.readlines()]

if __name__ == '__main__':
    input_lines = read_file('input.txt')
    print(solve(input_lines))