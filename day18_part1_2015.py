
def get_next_state(grid, i, j):
    on_neighbors = 0
    for x in range(i-1, i+2):
        for y in range(j-1, j+2):
            if 0 <= x < len(grid) and 0 <= y < len(grid[0]) and (x, y) != (i, j) and grid[x][y] == '#':
                on_neighbors += 1
    if grid[i][j] == '#' and on_neighbors not in (2, 3):
        return '.'
    elif grid[i][j] == '.' and on_neighbors == 3:
        return '#'
    else:
        return grid[i][j]

def animate(grid):
    new_grid = [['.' for _ in range(len(grid[0]))] for _ in range(len(grid))]
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            new_grid[i][j] = get_next_state(grid, i, j)
    return new_grid

with open('input.txt', 'r') as file:
    grid = [list(line.strip()) for line in file.readlines()]

for _ in range(100):
    grid = animate(grid)

result = sum(row.count('#') for row in grid)
print(result)
