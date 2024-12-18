
from collections import deque

def solve():
    grid_size = 71
    corrupted = set()
    
    with open("input.txt", "r") as f:
        for i, line in enumerate(f):
            if i >= 1024:
                break
            x, y = map(int, line.strip().split(","))
            corrupted.add((x, y))

    q = deque([(0, 0, 0)])
    visited = set()
    visited.add((0, 0))

    while q:
        x, y, steps = q.popleft()

        if x == grid_size - 1 and y == grid_size - 1:
            print(steps)
            return

        for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if 0 <= nx < grid_size and 0 <= ny < grid_size and (nx, ny) not in corrupted and (nx, ny) not in visited:
                q.append((nx, ny, steps + 1))
                visited.add((nx, ny))

solve()
