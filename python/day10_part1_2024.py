
import sys

def solve():
    with open("input.txt") as f:
        lines = f.read().splitlines()

    nr = len(lines)
    nc = len(lines[0])
    grid = [[int(c) for c in line] for line in lines]

    dirs = [(1,0),(-1,0),(0,1),(0,-1)]
    trailheads = [(r,c) for r in range(nr) for c in range(nc) if grid[r][c] == 0]

    sum_scores = 0
    for th in trailheads:
        reached = set()
        front = [(th, 0)]
        visited = set()
        while front:
            cur, h = front.pop()
            if h == 9:
                reached.add(cur)
                continue
            for dr, dc in dirs:
                nr2, nc2 = cur[0] + dr, cur[1] + dc
                if 0 <= nr2 < nr and 0 <= nc2 < nc:
                    if grid[nr2][nc2] == h + 1:
                        key = (nr2, nc2, h + 1)
                        if key not in visited:
                            visited.add(key)
                            front.append(((nr2, nc2), h + 1))
        sum_scores += len(reached)

    print(sum_scores)

solve()
