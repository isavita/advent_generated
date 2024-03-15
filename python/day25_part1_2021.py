with open("input.txt", "r") as f:
    lines = f.readlines()

def step(grid):
    new_grid = [list(row) for row in grid]
    moved = False
    
    for y in range(len(grid)):
        for x in range(len(grid[y])):
            if grid[y][x] == ">":
                if grid[y][(x+1)%len(grid[y])] == ".":
                    new_grid[y][x] = "."
                    new_grid[y][(x+1)%len(grid[y])] = ">"
                    moved = True
    
    grid = [row[:] for row in new_grid]
    new_grid = [list(row) for row in grid]
    
    for y in range(len(grid)):
        for x in range(len(grid[y])):
            if grid[y][x] == "v":
                if grid[(y+1)%len(grid)][x] == ".":
                    new_grid[y][x] = "."
                    new_grid[(y+1)%len(grid)][x] = "v"
                    moved = True
    
    return new_grid, moved

grid = [list(line.strip()) for line in lines]
steps = 0
while True:
    new_grid, moved = step(grid)
    if not moved:
        print(steps+1)
        break
    grid = new_grid
    steps += 1