import re

def read_input(filename):
    """
    Reads the input file and separates the map and the path instructions.
    """
    with open(filename, 'r') as f:
        lines = f.read().splitlines()
    
    # Separate map and path
    map_lines = []
    path = ""
    map_section = True
    for line in lines:
        if line.strip() == "":
            map_section = False
            continue
        if map_section:
            map_lines.append(line)
        else:
            path = line.strip()
    return map_lines, path

def parse_map(map_lines):
    """
    Parses the map lines into a 2D grid and computes row and column boundaries.
    """
    # Determine the maximum width
    max_width = max(len(line) for line in map_lines)
    num_rows = len(map_lines)
    num_cols = max_width

    # Pad each row with spaces to make them equal in length
    grid = []
    for line in map_lines:
        padded_line = line.ljust(max_width, ' ')
        grid.append(padded_line)
    
    # Precompute row boundaries (min_x and max_x for each row)
    row_boundaries = {}
    for y in range(1, num_rows + 1):
        row = grid[y - 1]
        min_x = None
        max_x = None
        for x in range(1, num_cols + 1):
            if row[x - 1] != ' ':
                if min_x is None:
                    min_x = x
                max_x = x
        row_boundaries[y] = (min_x, max_x)
    
    # Precompute column boundaries (min_y and max_y for each column)
    col_boundaries = {}
    for x in range(1, num_cols + 1):
        min_y = None
        max_y = None
        for y in range(1, num_rows + 1):
            if grid[y - 1][x - 1] != ' ':
                if min_y is None:
                    min_y = y
                max_y = y
        col_boundaries[x] = (min_y, max_y)
    
    return grid, row_boundaries, col_boundaries, num_rows, num_cols

def parse_path(path_str):
    """
    Parses the path string into a list of instructions (numbers and turns).
    """
    pattern = re.compile(r'(\d+|[LR])')
    instructions = pattern.findall(path_str)
    return instructions

def find_starting_position(grid):
    """
    Finds the starting position: the leftmost open tile of the top row.
    Returns (x, y).
    """
    top_row = grid[0]
    for x, tile in enumerate(top_row, start=1):
        if tile == '.':
            return (x, 1)
    raise ValueError("No starting position found.")

def turn_direction(current_facing, turn):
    """
    Updates the facing direction based on the turn instruction.
    """
    if turn == 'R':
        return (current_facing + 1) % 4
    elif turn == 'L':
        return (current_facing - 1) % 4
    else:
        raise ValueError("Invalid turn instruction.")

def move(position, direction):
    """
    Calculates the next position based on the current direction.
    Directions:
    0: Right, 1: Down, 2: Left, 3: Up
    """
    x, y = position
    if direction == 0:
        return (x + 1, y)
    elif direction == 1:
        return (x, y + 1)
    elif direction == 2:
        return (x - 1, y)
    elif direction == 3:
        return (x, y - 1)
    else:
        raise ValueError("Invalid direction.")

def simulate(grid, row_boundaries, col_boundaries, num_rows, num_cols, instructions):
    """
    Simulates the movement based on the instructions and returns the final position and facing.
    """
    # Find starting position
    current_x, current_y = find_starting_position(grid)
    facing = 0  # 0: Right

    for instr in instructions:
        if instr.isdigit():
            steps = int(instr)
            for _ in range(steps):
                next_x, next_y = move((current_x, current_y), facing)
                
                # Handle wrapping
                if facing == 0:  # Right
                    if next_x > row_boundaries[current_y][1]:
                        next_x = row_boundaries[current_y][0]
                elif facing == 2:  # Left
                    if next_x < row_boundaries[current_y][0]:
                        next_x = row_boundaries[current_y][1]
                elif facing == 1:  # Down
                    if next_y > col_boundaries[current_x][1]:
                        next_y = col_boundaries[current_x][0]
                elif facing == 3:  # Up
                    if next_y < col_boundaries[current_x][0]:
                        next_y = col_boundaries[current_x][1]
                
                # Check the tile at the next position
                tile = grid[next_y - 1][next_x - 1]
                if tile == '#':
                    # Wall encountered; stop moving for this instruction
                    break
                elif tile == '.':
                    # Open tile; move to the next position
                    current_x, current_y = next_x, next_y
                else:
                    # Invalid tile (space or undefined); should not happen
                    raise ValueError(f"Invalid tile encountered at ({next_x}, {next_y}): '{tile}'")
        elif instr in ('L', 'R'):
            facing = turn_direction(facing, instr)
        else:
            raise ValueError(f"Unknown instruction: {instr}")
    
    return (current_x, current_y, facing)

def compute_password(final_position):
    """
    Computes the final password based on the final position and facing.
    """
    x, y, facing = final_position
    return 1000 * y + 4 * x + facing

def main():
    # Read and parse input
    map_lines, path_str = read_input('input.txt')
    grid, row_boundaries, col_boundaries, num_rows, num_cols = parse_map(map_lines)
    instructions = parse_path(path_str)
    
    # Simulate movement
    final_position = simulate(grid, row_boundaries, col_boundaries, num_rows, num_cols, instructions)
    
    # Compute and print the final password
    password = compute_password(final_position)
    print(password)

if __name__ == "__main__":
    main()
