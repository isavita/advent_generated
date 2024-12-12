
from collections import deque

def solve():
    with open("input.txt") as f:
        grid = [line.strip() for line in f]

    rows, cols = len(grid), len(grid[0])
    visited = [[False] * cols for _ in range(rows)]
    total_price = 0

    for r in range(rows):
        for c in range(cols):
            if not visited[r][c]:
                area, perimeter = calculate_region(grid, r, c, visited)
                total_price += area * perimeter
    return total_price

def calculate_region(grid, row, col, visited):
    rows, cols = len(grid), len(grid[0])
    char = grid[row][col]
    area = 0
    perimeter = 0
    queue = deque([(row, col)])
    visited[row][col] = True

    while queue:
        r, c = queue.popleft()
        area += 1
        
        is_border = r == 0 or r == rows - 1 or c == 0 or c == cols - 1

        for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols:
                if grid[nr][nc] != char:
                    perimeter += 1
                elif not visited[nr][nc]:
                    visited[nr][nc] = True
                    queue.append((nr, nc))
            elif is_border:
                perimeter +=1

    return area, perimeter

print(solve())
