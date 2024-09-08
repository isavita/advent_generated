def solve_network_path(grid):
    directions = {
        'down': (1, 0),
        'up': (-1, 0),
        'right': (0, 1),
        'left': (0, -1)
    }

    def find_start():
        return 0, grid[0].index('|')

    def next_step(r, c, direction):
        dr, dc = directions[direction]
        nr, nc = r + dr, c + dc
        if 0 <= nr < len(grid) and 0 <= nc < len(grid[0]) and grid[nr][nc] != ' ':
            return nr, nc, direction
        for new_dir in directions:
            if new_dir != direction:
                dr, dc = directions[new_dir]
                nr, nc = r + dr, c + dc
                if 0 <= nr < len(grid) and 0 <= nc < len(grid[0]) and grid[nr][nc] != ' ':
                    return nr, nc, new_dir
        return None

    r, c = find_start()
    direction = 'down'
    letters = []
    steps = 0

    while True:
        steps += 1
        char = grid[r][c]
        if char.isalpha():
            letters.append(char)
        result = next_step(r, c, direction)
        if result is None:
            break
        r, c, direction = result

    return ''.join(letters), steps

# Read input from file
with open('input.txt', 'r') as file:
    grid = [line.rstrip('\n') for line in file]

# Solve the puzzle
letters, steps = solve_network_path(grid)

# Print the results
print(f"Part 1: {letters}")
print(f"Part 2: {steps}")
