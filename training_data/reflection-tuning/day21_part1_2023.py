from collections import deque

def count_reachable_plots(grid, steps):
    rows, cols = len(grid), len(grid[0])
    start = next((i, j) for i in range(rows) for j in range(cols) if grid[i][j] == 'S')
    
    queue = deque([(start, 0)])
    visited = set()
    final_positions = set()
    
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    
    while queue:
        (x, y), step = queue.popleft()
        
        if step == steps:
            final_positions.add((x, y))
            continue
        
        for dx, dy in directions:
            nx, ny = x + dx, y + dy
            if (0 <= nx < rows and 0 <= ny < cols and 
                grid[nx][ny] != '#' and 
                (nx, ny, step + 1) not in visited):
                visited.add((nx, ny, step + 1))
                queue.append(((nx, ny), step + 1))
    
    return len(final_positions)

# Read input from file
with open('input.txt', 'r') as file:
    grid = [line.strip() for line in file]

# Solve the problem
result = count_reachable_plots(grid, 64)

# Print the result
print(result)
