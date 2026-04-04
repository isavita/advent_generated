
#!/usr/bin/env bash
set -euo pipefail

python3 - <<'PY'
import heapq

with open("input.txt") as f:
    grid = [line.strip() for line in f if line.strip()]

h = len(grid)
w = len(grid[0])

dr = (-1, 1, 0, 0)
dc = (0, 0, 1, -1)

def solve(mn, mx):
    inf = 10**18
    dist = {}
    pq = []

    if w > 1:
        c = int(grid[0][1])
        dist[(0, 1, 2, 1)] = c
        heapq.heappush(pq, (c, 0, 1, 2, 1))
    if h > 1:
        c = int(grid[1][0])
        dist[(1, 0, 1, 1)] = c
        heapq.heappush(pq, (c, 1, 0, 1, 1))

    while pq:
        cost, r, c, d, s = heapq.heappop(pq)
        if cost != dist.get((r, c, d, s), inf):
            continue
        if r == h - 1 and c == w - 1 and s >= mn:
            return cost
        for nd in range(4):
            if (d == 0 and nd == 1) or (d == 1 and nd == 0) or (d == 2 and nd == 3) or (d == 3 and nd == 2):
                continue
            nr = r + dr[nd]
            nc = c + dc[nd]
            if nr < 0 or nr >= h or nc < 0 or nc >= w:
                continue
            if nd == d:
                if s >= mx:
                    continue
                ns = s + 1
            else:
                if s < mn:
                    continue
                ns = 1
            ncst = cost + ord(grid[nr][nc]) - 48
            key = (nr, nc, nd, ns)
            if ncst < dist.get(key, inf):
                dist[key] = ncst
                heapq.heappush(pq, (ncst, nr, nc, nd, ns))
    return -1

print(solve(1, 3))
print(solve(4, 10))
PY
