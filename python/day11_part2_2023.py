from dataclasses import dataclass, field
from typing import Dict, List

@dataclass(frozen=True)
class Coord:
    x: int
    y: int

    def __hash__(self):
        return hash((self.x, self.y))

    def __eq__(self, other):
        return (self.x, self.y) == (other.x, other.y)

@dataclass
class Grid:
    width: int
    height: int
    data: Dict[Coord, str] = field(default_factory=dict)

EMPTY = '.'

def build_grid(input_lines: List[str], empty: str) -> Grid:
    grid = Grid(
        width=len(input_lines[0]),
        height=len(input_lines),
    )

    for y, line in enumerate(input_lines):
        for x, char in enumerate(line):
            if char != empty:
                grid.data[Coord(x, y)] = char

    return grid

def to_string(grid: Grid, empty: str) -> str:
    result = []
    for y in range(grid.height):
        row = []
        for x in range(grid.width):
            coord = Coord(x, y)
            if coord in grid.data:
                row.append(grid.data[coord])
            else:
                row.append(empty)
        result.append(''.join(row))
    return '\n'.join(result)

def get_empty_rows(grid: Grid) -> List[int]:
    empty_rows = []
    for y in range(grid.height):
        is_empty = True
        for x in range(grid.width):
            if Coord(x, y) in grid.data:
                is_empty = False
                break
        if is_empty:
            empty_rows.append(y)
    return empty_rows

def get_empty_cols(grid: Grid) -> List[int]:
    empty_cols = []
    for x in range(grid.width):
        is_empty = True
        for y in range(grid.height):
            if Coord(x, y) in grid.data:
                is_empty = False
                break
        if is_empty:
            empty_cols.append(x)
    return empty_cols

def calculate_offsets(empty_indexes: List[int], bound: int) -> List[int]:
    offsets = [0] * bound
    for idx in empty_indexes:
        for i in range(idx + 1, len(offsets)):
            offsets[i] += 1
    return offsets

def expand_grid(grid: Grid, expansion_factor: int) -> Grid:
    empty_cols = get_empty_cols(grid)
    empty_rows = get_empty_rows(grid)
    num_lines_to_add = expansion_factor - 1

    new_grid = Grid(
        width=grid.width + len(empty_cols) * num_lines_to_add,
        height=grid.height + len(empty_rows) * num_lines_to_add,
    )

    dx = calculate_offsets(empty_cols, grid.width)
    dy = calculate_offsets(empty_rows, grid.height)

    for y in range(grid.height):
        for x in range(grid.width):
            coord = Coord(x, y)
            if coord in grid.data:
                new_coord = Coord(x + dx[x] * num_lines_to_add, y + dy[y] * num_lines_to_add)
                new_grid.data[new_coord] = grid.data[coord]

    return new_grid

def abs(x: int) -> int:
    return -x if x < 0 else x

def calculate_length(grid: Grid, c1: Coord, c2: Coord) -> int:
    dx = abs(c2.x - c1.x)
    dy = abs(c2.y - c1.y)
    return dx + dy

def solve(input_lines: List[str], expansion_factor: int) -> int:
    grid = build_grid(input_lines, EMPTY)
    expanded_grid = expand_grid(grid, expansion_factor)

    res = 0
    already_seen = set()
    for coord1 in expanded_grid.data:
        for coord2 in already_seen:
            length = calculate_length(expanded_grid, coord1, coord2)
            res += length
        already_seen.add(coord1)

    return res

def read_file(file_name: str) -> List[str]:
    with open(file_name, 'r') as file:
        return [line.strip() for line in file.readlines()]

if __name__ == '__main__':
    input_lines = read_file('input.txt')
    print(solve(input_lines, 1000000))