def count_neighbors(grid, x, y):
    return sum(grid[i][j] for i in range(x-1, x+2) for j in range(y-1, y+2) if (i, j) != (x, y))

def update_grid(grid):
    new_grid = [row[:] for row in grid]
    for x in range(1, len(grid)-1):
        for y in range(1, len(grid[0])-1):
            neighbors = count_neighbors(grid, x, y)
            if grid[x][y]:
                new_grid[x][y] = neighbors in (2, 3)
            else:
                new_grid[x][y] = neighbors == 3
    return new_grid

# Read input
with open('input.txt', 'r') as f:
    initial_grid = [[c == '#' for c in line.strip()] for line in f]

# Add border
size = len(initial_grid)
grid = [[False] * (size + 2) for _ in range(size + 2)]
for i in range(size):
    for j in range(size):
        grid[i+1][j+1] = initial_grid[i][j]

# Simulate 100 steps
for _ in range(100):
    grid = update_grid(grid)

# Count lights on
lights_on = sum(sum(row) for row in grid)

print(lights_on)
