
from collections import Counter

def get_adjacent(x, y, grid):
    adj = []
    for i in range(x-1, x+2):
        for j in range(y-1, y+2):
            if (i,j) != (x,y) and 0 <= i < len(grid) and 0 <= j < len(grid[0]):
                adj.append(grid[i][j])
    return Counter(adj)

def update_area(grid):
    new_grid = [['']*len(grid[0]) for _ in range(len(grid))]
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            count = get_adjacent(i, j, grid)
            if grid[i][j] == '.':
                new_grid[i][j] = '|' if count['|'] >= 3 else '.'
            elif grid[i][j] == '|':
                new_grid[i][j] = '#' if count['#'] >= 3 else '|'
            elif grid[i][j] == '#':
                new_grid[i][j] = '#' if count['#'] >= 1 and count['|'] >= 1 else '.'
    return new_grid

def resource_value(grid):
    wooded = sum(row.count('|') for row in grid)
    lumberyards = sum(row.count('#') for row in grid)
    return wooded * lumberyards

with open('input.txt', 'r') as file:
    grid = [list(line.strip()) for line in file.readlines()]

for _ in range(10):
    grid = update_area(grid)

print(resource_value(grid))
