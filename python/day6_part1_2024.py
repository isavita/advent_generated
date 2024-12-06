# read the input file
with open('input.txt', 'r') as file:
    input_str = file.read()

# implement the guard's path prediction algorithm
def predict_guard_path(input_str):
    # Split input into grid
    grid = [list(line) for line in input_str.split('\n')]
    h, w = len(grid), len(grid[0])
    
    # Define directions: up, right, down, left
    dirs = [(0, -1), (1, 0), (0, 1), (-1, 0)]
    dir_idx = 0  # Start facing up
    
    # Find starting position
    x = y = None
    for i in range(h):
        for j in range(w):
            if grid[i][j] == '^':
                x, y = j, i
                break
        if x is not None:
            break
    
    # Track visited positions
    visited = {(x, y)}
    dir_x, dir_y = dirs[dir_idx]
    
    # Follow path until leaving grid
    while True:
        nx, ny = x + dir_x, y + dir_y
        
        # Check if we're leaving the grid
        if nx < 0 or nx >= w or ny < 0 or ny >= h:
            break
            
        # If obstacle found, turn right
        if grid[ny][nx] == '#':
            dir_idx = (dir_idx + 1) % 4
            dir_x, dir_y = dirs[dir_idx]
            continue
            
        # Move to new position
        x, y = nx, ny
        visited.add((x, y))
    
    return len(visited)

print(predict_guard_path(input_str))
