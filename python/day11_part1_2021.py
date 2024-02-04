
def read_input(filename):
    with open(filename, 'r') as file:
        grid = []
        for line in file:
            row = [int(char) for char in line.strip()]
            grid.append(row)
    return grid

def simulate_step(grid):
    flashes = 0
    flashed = {}

    for y in range(len(grid)):
        for x in range(len(grid[0])):
            grid[y][x] += 1

    for y in range(len(grid)):
        for x in range(len(grid[0])):
            if grid[y][x] > 9:
                flashes += flash(grid, x, y, flashed)

    for coords in flashed:
        grid[coords[1]][coords[0]] = 0

    return flashes

def flash(grid, x, y, flashed):
    if (x, y) in flashed:
        return 0

    flashed[(x, y)] = True
    flashes = 1
    directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

    for dir in directions:
        new_x, new_y = x + dir[0], y + dir[1]
        if 0 <= new_x < len(grid[0]) and 0 <= new_y < len(grid):
            grid[new_y][new_x] += 1
            if grid[new_y][new_x] > 9:
                flashes += flash(grid, new_x, new_y, flashed)

    return flashes

if __name__ == "__main__":
    grid = read_input("input.txt")
    total_flashes = 0
    for step in range(100):
        total_flashes += simulate_step(grid)

    print(total_flashes)
