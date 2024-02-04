
def read_input(filename):
    with open(filename, 'r') as file:
        grid = []
        for line in file:
            row = [int(char) for char in line.strip()]
            grid.append(row)
    return grid

def simulate_step(grid):
    flashes = 0
    flashed = set()

    for y in range(len(grid)):
        for x in range(len(grid[y])):
            grid[y][x] += 1

    for y in range(len(grid)):
        for x in range(len(grid[y])):
            if grid[y][x] > 9:
                flashes += flash(grid, x, y, flashed)

    for coords in flashed:
        x, y = coords
        grid[y][x] = 0

    return flashes

def flash(grid, x, y, flashed):
    if (x, y) in flashed:
        return 0

    flashed.add((x, y))
    flashes = 1
    directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

    for dx, dy in directions:
        new_x, new_y = x + dx, y + dy
        if 0 <= new_x < len(grid[0]) and 0 <= new_y < len(grid):
            grid[new_y][new_x] += 1
            if grid[new_y][new_x] > 9:
                flashes += flash(grid, new_x, new_y, flashed)

    return flashes

grid = read_input("input.txt")
step = 0
while True:
    step += 1
    flashes = simulate_step(grid)
    if flashes == 100:
        break

print(step)
