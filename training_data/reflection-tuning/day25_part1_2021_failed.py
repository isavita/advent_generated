def move_east(grid):
    rows, cols = len(grid), len(grid[0])
    moved = False
    new_grid = [row[:] for row in grid]
    
    for i in range(rows):
        for j in range(cols):
            if grid[i][j] == '>':
                next_j = (j + 1) % cols
                if grid[i][next_j] == '.':
                    new_grid[i][j] = '.'
                    new_grid[i][next_j] = '>'
                    moved = True
    
    return new_grid, moved

def move_south(grid):
    rows, cols = len(grid), len(grid[0])
    moved = False
    new_grid = [row[:] for row in grid]
    
    for i in range(rows):
        for j in range(cols):
            if grid[i][j] == 'v':
                next_i = (i + 1) % rows
                if grid[next_i][j] == '.':
                    new_grid[i][j] = '.'
                    new_grid[next_i][j] = 'v'
                    moved = True
    
    return new_grid, moved

def simulate(grid):
    steps = 0
    while True:
        steps += 1
        grid, moved_east = move_east(grid)
        grid, moved_south = move_south(grid)
        if not (moved_east or moved_south):
            return steps

def solve(input_data):
    grid = [list(line.strip()) for line in input_data.split('\n') if line.strip()]
    return simulate(grid)

# Example usage:
input_data = """
v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
"""

result = solve(input_data)
print(result)
