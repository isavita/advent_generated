def read_input(filename):
    with open(filename, 'r') as file:
        return [list(line.strip()) for line in file]

def count_neighbors(grid, x, y):
    neighbors = [
        grid[i][j]
        for i in range(max(0, x-1), min(len(grid), x+2))
        for j in range(max(0, y-1), min(len(grid[0]), y+2))
        if (i, j) != (x, y)
    ]
    return {'.': neighbors.count('.'), '|': neighbors.count('|'), '#': neighbors.count('#')}

def simulate_minute(grid):
    new_grid = [row[:] for row in grid]
    for x in range(len(grid)):
        for y in range(len(grid[0])):
            counts = count_neighbors(grid, x, y)
            if grid[x][y] == '.' and counts['|'] >= 3:
                new_grid[x][y] = '|'
            elif grid[x][y] == '|' and counts['#'] >= 3:
                new_grid[x][y] = '#'
            elif grid[x][y] == '#' and not (counts['#'] >= 1 and counts['|'] >= 1):
                new_grid[x][y] = '.'
    return new_grid

def calculate_resource_value(grid):
    flat_grid = [item for sublist in grid for item in sublist]
    return flat_grid.count('|') * flat_grid.count('#')

def solve(grid, minutes):
    seen = {}
    minute = 0
    while minute < minutes:
        grid_tuple = tuple(tuple(row) for row in grid)
        if grid_tuple in seen:
            cycle_length = minute - seen[grid_tuple]
            remaining = (minutes - minute) % cycle_length
            return solve(grid, remaining)
        seen[grid_tuple] = minute
        grid = simulate_minute(grid)
        minute += 1
    return calculate_resource_value(grid)

grid = read_input("input.txt")
print("Part 1:", solve(grid, 10))
print("Part 2:", solve(grid, 1000000000))
