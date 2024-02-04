
gridSize = 100
steps = 100

def count_on_neighbors(grid, x, y):
    on = 0
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            if dx == 0 and dy == 0:
                continue
            nx, ny = x+dx, y+dy
            if 0 <= nx < gridSize and 0 <= ny < gridSize and grid[nx][ny]:
                on += 1
    return on

def step(grid):
    new_grid = [[False for _ in range(gridSize)] for _ in range(gridSize)]

    for x in range(gridSize):
        for y in range(gridSize):
            on_neighbors = count_on_neighbors(grid, x, y)
            if grid[x][y]:
                new_grid[x][y] = on_neighbors == 2 or on_neighbors == 3
            else:
                new_grid[x][y] = on_neighbors == 3

    new_grid[0][0] = True
    new_grid[0][gridSize-1] = True
    new_grid[gridSize-1][0] = True
    new_grid[gridSize-1][gridSize-1] = True

    return new_grid

grid = [[False for _ in range(gridSize)] for _ in range(gridSize)]

with open("input.txt", "r") as file:
    y = 0
    for line in file:
        for x, c in enumerate(line.strip()):
            grid[x][y] = c == '#'
        y += 1

grid[0][0] = True
grid[0][gridSize-1] = True
grid[gridSize-1][0] = True
grid[gridSize-1][gridSize-1] = True

for _ in range(steps):
    grid = step(grid)

on_count = sum(light for row in grid for light in row)
print(on_count)
