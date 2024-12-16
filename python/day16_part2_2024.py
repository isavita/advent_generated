
import heapq

def solve():
    with open("input.txt", "r") as f:
        grid = [line.strip() for line in f]

    n, m = len(grid), len(grid[0])
    for r in range(n):
        for c in range(m):
            if grid[r][c] == 'S':
                sx, sy = r, c
            elif grid[r][c] == 'E':
                ex, ey = r, c

    dx = [-1, 0, 1, 0]
    dy = [0, 1, 0, -1]

    dist = [[[float('inf')] * 4 for _ in range(m)] for _ in range(n)]
    dist[sx][sy][1] = 0

    pq = [(0, sx, sy, 1)]

    while pq:
        cost, x, y, d = heapq.heappop(pq)
        if cost > dist[x][y][d]:
            continue

        for ndir in [(d + 1) % 4, (d + 3) % 4]:
            nc = cost + 1000
            if nc < dist[x][y][ndir]:
                dist[x][y][ndir] = nc
                heapq.heappush(pq, (nc, x, y, ndir))

        nx, ny = x + dx[d], y + dy[d]
        if 0 <= nx < n and 0 <= ny < m and grid[nx][ny] != '#':
            nc = cost + 1
            if nc < dist[nx][ny][d]:
                dist[nx][ny][d] = nc
                heapq.heappush(pq, (nc, nx, ny, d))

    best = min(dist[ex][ey])

    used = [[False] * m for _ in range(n)]
    rev = [(ex, ey, d) for d in range(4) if dist[ex][ey][d] == best]
    vis = [[[False] * 4 for _ in range(m)] for _ in range(n)]
    for x, y, d in rev:
        vis[x][y][d] = True

    while rev:
        x, y, d = rev.pop()
        used[x][y] = True
        costU = dist[x][y][d]

        for pd in [(d + 1) % 4, (d + 3) % 4]:
            if dist[x][y][pd] == costU - 1000 and not vis[x][y][pd]:
                vis[x][y][pd] = True
                rev.append((x, y, pd))

        px, py = x - dx[d], y - dy[d]
        if 0 <= px < n and 0 <= py < m and grid[px][py] != '#' and dist[px][py][d] == costU - 1 and not vis[px][py][d]:
            vis[px][py][d] = True
            rev.append((px, py, d))

    count = sum(sum(row) for row in used if any(row))
    print(count)

solve()
