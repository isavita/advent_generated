from collections import Counter

def read_input(filename):
    with open(filename, 'r') as file:
        return [list(line.strip()) for line in file]

def count_neighbors(grid, x, y):
    directions = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]
    neighbors = Counter()
    for dx, dy in directions:
        nx, ny = x + dx, y + dy
        if 0 <= nx < len(grid) and 0 <= ny < len(grid[0]):
            neighbors[grid[nx][ny]] += 1
    return neighbors

def apply_rules(acre, neighbors):
    if acre == '.' and neighbors['|'] >= 3:
        return '|'
    elif acre == '|' and neighbors['#'] >= 3:
        return '#'
    elif acre == '#' and (neighbors['#'] < 1 or neighbors['|'] < 1):
        return '.'
    return acre

def simulate_minute(grid):
    return [[apply_rules(grid[x][y], count_neighbors(grid, x, y))
             for y in range(len(grid[0]))]
            for x in range(len(grid))]

def count_resources(grid):
    flat_grid = [acre for row in grid for acre in row]
    counts = Counter(flat_grid)
    return counts['|'] * counts['#']

def solve(filename, minutes):
    grid = read_input(filename)
    for _ in range(minutes):
        grid = simulate_minute(grid)
    return count_resources(grid)

print(solve('input.txt', 10))
