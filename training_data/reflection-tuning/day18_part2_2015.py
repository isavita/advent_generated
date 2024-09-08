def read_input(filename):
    with open(filename, 'r') as f:
        return [list(line.strip()) for line in f]

def count_neighbors(grid, x, y):
    neighbors = 0
    for dx in [-1, 0, 1]:
        for dy in [-1, 0, 1]:
            if dx == 0 and dy == 0:
                continue
            nx, ny = x + dx, y + dy
            if 0 <= nx < len(grid) and 0 <= ny < len(grid[0]):
                neighbors += grid[nx][ny] == '#'
    return neighbors

def update_grid(grid):
    new_grid = [row[:] for row in grid]
    for x in range(len(grid)):
        for y in range(len(grid[0])):
            neighbors = count_neighbors(grid, x, y)
            if grid[x][y] == '#':
                new_grid[x][y] = '#' if neighbors in [2, 3] else '.'
            else:
                new_grid[x][y] = '#' if neighbors == 3 else '.'
    return new_grid

def update_grid_part2(grid):
    new_grid = update_grid(grid)
    n = len(grid) - 1
    new_grid[0][0] = new_grid[0][n] = new_grid[n][0] = new_grid[n][n] = '#'
    return new_grid

def simulate(grid, steps, update_func):
    for _ in range(steps):
        grid = update_func(grid)
    return sum(row.count('#') for row in grid)

def solve_part1(grid):
    return simulate(grid, 100, update_grid)

def solve_part2(grid):
    n = len(grid) - 1
    grid[0][0] = grid[0][n] = grid[n][0] = grid[n][n] = '#'
    return simulate(grid, 100, update_grid_part2)

grid = read_input('input.txt')
print(f"Part 1: {solve_part1(grid)}")
print(f"Part 2: {solve_part2(grid)}")
