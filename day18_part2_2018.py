
Open = '.'
Trees = '|'
Lumberyard = '#'
Size = 50

def read_input(filename):
    with open(filename, 'r') as file:
        grid = [list(line.strip()) for line in file]
    return grid

def transform(grid):
    new_grid = [[None for _ in range(len(grid[0]))] for _ in range(len(grid))]
    for i in range(len(grid)):
        for j in range(len(grid[i])):
            new_grid[i][j] = next_acre_state(grid, i, j)
    return new_grid

def next_acre_state(grid, i, j):
    acre_type = grid[i][j]
    if acre_type == Open:
        if count_adjacent(grid, i, j, Trees) >= 3:
            return Trees
    elif acre_type == Trees:
        if count_adjacent(grid, i, j, Lumberyard) >= 3:
            return Lumberyard
    elif acre_type == Lumberyard:
        if count_adjacent(grid, i, j, Lumberyard) >= 1 and count_adjacent(grid, i, j, Trees) >= 1:
            return Lumberyard
        return Open
    return acre_type

def count_adjacent(grid, i, j, acre_type):
    count = 0
    for x in range(-1, 2):
        for y in range(-1, 2):
            if x == 0 and y == 0:
                continue
            if 0 <= i + x < len(grid) and 0 <= j + y < len(grid[i]) and grid[i + x][j + y] == acre_type:
                count += 1
    return count

def count_resources(grid):
    wooded, lumberyards = 0, 0
    for row in grid:
        for acre in row:
            if acre == Trees:
                wooded += 1
            elif acre == Lumberyard:
                lumberyards += 1
    return wooded, lumberyards

def grid_to_string(grid):
    return '\n'.join([''.join(row) for row in grid])

grid = read_input("input.txt")
seen_states = {}
cycle_start, cycle_length = 0, 0

for minute in range(1000000000):
    state = grid_to_string(grid)
    if state in seen_states:
        cycle_start = seen_states[state]
        cycle_length = minute - seen_states[state]
        break
    seen_states[state] = minute
    grid = transform(grid)

remaining_minutes = (1000000000 - cycle_start) % cycle_length
for _ in range(remaining_minutes):
    grid = transform(grid)

wooded, lumberyards = count_resources(grid)
print(wooded * lumberyards)
