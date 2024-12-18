
from collections import deque

def solve():
    with open("input.txt", "r") as f:
        byte_positions = [tuple(map(int, line.strip().split(","))) for line in f]

    grid_size = 71
    
    def is_valid(r, c, grid):
        return 0 <= r < grid_size and 0 <= c < grid_size and grid[r][c] == '.'

    def bfs(grid):
        q = deque([(0, 0, 0)])
        visited = set()
        visited.add((0, 0))

        while q:
            r, c, dist = q.popleft()
            if r == grid_size - 1 and c == grid_size - 1:
                return dist

            for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
                nr, nc = r + dr, c + dc
                if is_valid(nr, nc, grid) and (nr, nc) not in visited:
                    visited.add((nr, nc))
                    q.append((nr, nc, dist + 1))
        return -1

    # Part 1
    grid = [['.' for _ in range(grid_size)] for _ in range(grid_size)]
    for x, y in byte_positions[:1024]:
        grid[y][x] = '#'
    
    part1_result = bfs(grid)
    print(part1_result)

    # Part 2
    grid = [['.' for _ in range(grid_size)] for _ in range(grid_size)]
    for i, (x, y) in enumerate(byte_positions):
        grid[y][x] = '#'
        if bfs(grid) == -1:
            print(f"{x},{y}")
            break

solve()
