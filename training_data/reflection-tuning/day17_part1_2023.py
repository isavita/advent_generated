import heapq
from typing import List, Tuple

# Constants for directions
UP, RIGHT, DOWN, LEFT = (-1, 0), (0, 1), (1, 0), (0, -1)
DIRECTIONS = [UP, RIGHT, DOWN, LEFT]

def min_heat_loss(grid: List[List[int]]) -> int:
    rows, cols = len(grid), len(grid[0])
    target = (rows - 1, cols - 1)
    
    # Priority queue: (heat_loss, row, col, direction, steps_in_direction)
    pq = [(0, 0, 0, None, 0)]
    visited = set()
    
    while pq:
        heat_loss, row, col, direction, steps = heapq.heappop(pq)
        
        if (row, col) == target:
            return heat_loss
        
        if (row, col, direction, steps) in visited:
            continue
        visited.add((row, col, direction, steps))
        
        for new_dir in DIRECTIONS:
            if new_dir == direction and steps == 3:
                continue
            if direction and new_dir == (-direction[0], -direction[1]):
                continue
            
            new_row, new_col = row + new_dir[0], col + new_dir[1]
            if 0 <= new_row < rows and 0 <= new_col < cols:
                new_steps = 1 if new_dir != direction else steps + 1
                new_heat_loss = heat_loss + grid[new_row][new_col]
                heapq.heappush(pq, (new_heat_loss, new_row, new_col, new_dir, new_steps))
    
    return -1  # No path found

# Read input from file
with open('input.txt', 'r') as file:
    grid = [[int(char) for char in line.strip()] for line in file]

# Solve and print the result
print(min_heat_loss(grid))
