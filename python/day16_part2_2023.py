import sys
from collections import deque

def read_input(file_path):
    """
    Reads the input file and parses the grid.

    Args:
        file_path (str): Path to the input file.

    Returns:
        grid (list of list of str): 2D grid representing the contraption.
    """
    grid = []
    with open(file_path, 'r') as file:
        for line in file:
            grid.append(list(line.strip()))
    return grid

def get_edge_start_positions(grid):
    """
    Generates all possible starting positions and their initial directions
    based on the edges of the grid.

    Args:
        grid (list of list of str): 2D grid representing the contraption.

    Returns:
        starts (list of tuples): List of tuples containing starting position (x, y)
                                 and initial direction (dx, dy).
    """
    starts = []
    height = len(grid)
    width = len(grid[0])

    # Top row (y = 0), direction down
    for x in range(width):
        if grid[0][x] != '#':
            starts.append(((x, 0), (0, 1)))  # Down

    # Bottom row (y = height-1), direction up
    for x in range(width):
        if grid[height-1][x] != '#':
            starts.append(((x, height-1), (0, -1)))  # Up

    # Left column (x = 0), direction right
    for y in range(1, height-1):
        if grid[y][0] != '#':
            starts.append(((0, y), (1, 0)))  # Right

    # Right column (x = width-1), direction left
    for y in range(1, height-1):
        if grid[y][width-1] != '#':
            starts.append(((width-1, y), (-1, 0)))  # Left

    return starts

def reflect(direction, mirror):
    """
    Reflects the beam direction based on the mirror type.

    Args:
        direction (tuple): Current direction (dx, dy).
        mirror (str): Mirror type ('/' or '\\').

    Returns:
        new_direction (tuple): Reflected direction (dx, dy).
    """
    dx, dy = direction
    if mirror == '/':
        return (-dy, -dx)
    elif mirror == '\\':
        return (dy, dx)
    else:
        return direction

def split_beam(direction, splitter):
    """
    Splits the beam based on the splitter type and current direction.

    Args:
        direction (tuple): Current direction (dx, dy).
        splitter (str): Splitter type ('|' or '-').

    Returns:
        new_directions (list of tuples): List of new directions after splitting.
    """
    dx, dy = direction
    if splitter == '|':
        if dx != 0:  # Moving horizontally, split vertically
            return [(0, -1), (0, 1)]  # Up and Down
    elif splitter == '-':
        if dy != 0:  # Moving vertically, split horizontally
            return [(-1, 0), (1, 0)]  # Left and Right
    return []  # No split

def simulate_beam(grid, start_pos, start_dir):
    """
    Simulates the beam(s) starting from a given position and direction.

    Args:
        grid (list of list of str): 2D grid representing the contraption.
        start_pos (tuple): Starting position (x, y).
        start_dir (tuple): Initial direction (dx, dy).

    Returns:
        energized (set of tuples): Set of positions (x, y) that are energized.
    """
    height = len(grid)
    width = len(grid[0])
    queue = deque()
    visited = set()
    energized = set()

    queue.append((start_pos[0], start_pos[1], start_dir))
    while queue:
        x, y, direction = queue.popleft()
        state = (x, y, direction)
        if state in visited:
            continue
        visited.add(state)
        energized.add((x, y))

        dx, dy = direction
        nx, ny = x + dx, y + dy

        # Check if the beam is still within the grid
        if not (0 <= nx < width and 0 <= ny < height):
            continue  # Beam has left the grid

        cell = grid[ny][nx]

        if cell == '.':
            queue.append((nx, ny, direction))
        elif cell in {'/', '\\'}:
            new_dir = reflect(direction, cell)
            queue.append((nx, ny, new_dir))
        elif cell in {'|', '-'}:
            split_dirs = split_beam(direction, cell)
            if split_dirs:
                for new_dir in split_dirs:
                    queue.append((nx, ny, new_dir))
            else:
                queue.append((nx, ny, direction))
        else:
            # Unknown cell type, treat as empty
            queue.append((nx, ny, direction))

    return energized

def part_two(grid):
    """
    Solves Part Two of the challenge: Finds the starting configuration
    that energizes the largest number of tiles.

    Args:
        grid (list of list of str): 2D grid representing the contraption.

    Returns:
        max_energized (int): Maximum number of tiles that can be energized.
    """
    starts = get_edge_start_positions(grid)
    max_energized = 0

    for start_pos, start_dir in starts:
        energized = simulate_beam(grid, start_pos, start_dir)
        count = len(energized)
        if count > max_energized:
            max_energized = count

    return max_energized

def main():
    input_file = 'input.txt'
    grid = read_input(input_file)

    # Part One: Beam starts at top-left corner, heading right
    part_one_start_pos = (0, 0)
    part_one_start_dir = (1, 0)  # Right
    energized_part_one = simulate_beam(grid, part_one_start_pos, part_one_start_dir)
    print("Part One Answer:", len(energized_part_one))

    # Part Two: Find the starting configuration that energizes the most tiles
    max_energized_part_two = part_two(grid)
    print("Part Two Answer:", max_energized_part_two)

if __name__ == "__main__":
    main()
