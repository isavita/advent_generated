
def get_adjacent_bugs(grid, x, y):
    count = 0
    for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        nx, ny = x + dx, y + dy
        if 0 <= nx < 5 and 0 <= ny < 5 and grid[ny][nx] == '#':
            count += 1
    return count

def update_grid(grid):
    new_grid = [['.' for _ in range(5)] for _ in range(5)]
    for y in range(5):
        for x in range(5):
            adj_bugs = get_adjacent_bugs(grid, x, y)
            if grid[y][x] == '#' and adj_bugs != 1:
                new_grid[y][x] = '.'
            elif grid[y][x] == '.' and adj_bugs in [1, 2]:
                new_grid[y][x] = '#'
            else:
                new_grid[y][x] = grid[y][x]
    return new_grid

def get_biodiversity_rating(grid):
    rating = 0
    for y in range(5):
        for x in range(5):
            if grid[y][x] == '#':
                rating += 2**(y*5 + x)
    return rating

grid = []
with open('input.txt') as f:
    for line in f:
        grid.append(list(line.strip()))

seen = set()
while True:
    rating = get_biodiversity_rating(grid)
    if rating in seen:
        print(rating)
        break
    seen.add(rating)
    grid = update_grid(grid)
