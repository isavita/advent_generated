
import heapq

def solve():
    with open("input.txt", "r") as f:
        grid = [list(line.strip()) for line in f]

    rows = len(grid)
    cols = len(grid[0])

    start_row, start_col = -1, -1
    end_row, end_col = -1, -1

    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == 'S':
                start_row, start_col = r, c
            elif grid[r][c] == 'E':
                end_row, end_col = r, c

    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]  # East, South, West, North
    
    def get_neighbors(r, c, direction_index):
        neighbors = []
        
        # Move forward
        dr, dc = directions[direction_index]
        nr, nc = r + dr, c + dc
        if 0 <= nr < rows and 0 <= nc < cols and grid[nr][nc] != '#':
            neighbors.append((nr, nc, direction_index, 1))
        
        # Rotate clockwise
        new_direction_index = (direction_index + 1) % 4
        neighbors.append((r, c, new_direction_index, 1000))
        
        # Rotate counter-clockwise
        new_direction_index = (direction_index - 1 + 4) % 4
        neighbors.append((r, c, new_direction_index, 1000))
        
        return neighbors

    
    visited = set()
    priority_queue = [(0, start_row, start_col, 0)]  # (cost, row, col, direction_index)
    
    while priority_queue:
        cost, r, c, direction_index = heapq.heappop(priority_queue)
        
        if (r, c, direction_index) in visited:
            continue
        visited.add((r, c, direction_index))
        
        if r == end_row and c == end_col:
            print(cost)
            return
        
        for nr, nc, new_direction_index, move_cost in get_neighbors(r, c, direction_index):
            heapq.heappush(priority_queue, (cost + move_cost, nr, nc, new_direction_index))

solve()
