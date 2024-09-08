def parse_input(filename):
    with open(filename, 'r') as f:
        map_data, path = f.read().split('\n\n')
    
    map_lines = map_data.split('\n')
    max_width = max(len(line) for line in map_lines)
    map_grid = [list(line.ljust(max_width)) for line in map_lines]
    
    return map_grid, path.strip()

def find_start(map_grid):
    return (0, map_grid[0].index('.'))

def move(map_grid, pos, direction):
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]  # R, D, L, U
    row, col = pos
    dr, dc = directions[direction]
    new_row, new_col = (row + dr) % len(map_grid), (col + dc) % len(map_grid[0])
    
    while map_grid[new_row][new_col] == ' ':
        new_row, new_col = (new_row + dr) % len(map_grid), (new_col + dc) % len(map_grid[0])
    
    if map_grid[new_row][new_col] == '#':
        return pos
    return (new_row, new_col)

def solve(map_grid, path):
    pos = find_start(map_grid)
    direction = 0
    
    steps = ''
    for char in path + 'X':  # Add dummy char to process last number
        if char.isdigit():
            steps += char
        else:
            if steps:
                for _ in range(int(steps)):
                    pos = move(map_grid, pos, direction)
                steps = ''
            if char == 'R':
                direction = (direction + 1) % 4
            elif char == 'L':
                direction = (direction - 1) % 4
    
    row, col = pos
    return 1000 * (row + 1) + 4 * (col + 1) + direction

map_grid, path = parse_input("input.txt")
result = solve(map_grid, path)
print(result)
