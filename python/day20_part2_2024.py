
from collections import deque

def solve():
    with open("input.txt", "r") as f:
        grid = [line.strip() for line in f]

    h, w = len(grid), len(grid[0])
    walls = [[False] * w for _ in range(h)]
    track_cells = []
    for i in range(h):
        for j in range(w):
            if grid[i][j] == 'S':
                S = (i, j)
            elif grid[i][j] == 'E':
                E = (i, j)
            if grid[i][j] == '#':
                walls[i][j] = True
            else:
                track_cells.append((i, j))

    dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

    def is_track(x, y):
        return 0 <= x < h and 0 <= y < w and not walls[x][y]

    def bfs(start):
        dist = [[-1] * w for _ in range(h)]
        dist[start[0]][start[1]] = 0
        q = deque([start])
        while q:
            x, y = q.popleft()
            for dx, dy in dirs:
                nx, ny = x + dx, y + dy
                if 0 <= nx < h and 0 <= ny < w and not walls[nx][ny] and dist[nx][ny] == -1:
                    dist[nx][ny] = dist[x][y] + 1
                    q.append((nx, ny))
        return dist

    dist_from_s = bfs(S)
    dist_from_e = bfs(E)
    if dist_from_s[E[0]][E[1]] == -1:
        print(0)
        return

    normal_cost = dist_from_s[E[0]][E[1]]
    cheats = {}

    for start_x, start_y in track_cells:
        sd = dist_from_s[start_x][start_y]
        if sd == -1:
            continue

        dist_c = [[-1] * w for _ in range(h)]
        dist_c[start_x][start_y] = 0
        q = deque([(start_x, start_y)])

        while q:
            x, y = q.popleft()
            steps = dist_c[x][y]
            if steps == 20:
                continue
            for dx, dy in dirs:
                nx, ny = x + dx, y + dy
                if 0 <= nx < h and 0 <= ny < w and dist_c[nx][ny] == -1:
                    dist_c[nx][ny] = steps + 1
                    q.append((nx, ny))

        for x in range(h):
            for y in range(w):
                s = dist_c[x][y]
                if 0 < s <= 20 and is_track(x, y):
                    ed = dist_from_e[x][y]
                    if ed == -1:
                        continue
                    cost = sd + s + ed
                    if cost < normal_cost:
                        key = (start_x, start_y, x, y)
                        if key not in cheats or cost < cheats[key]:
                            cheats[key] = cost

    count = 0
    for cost in cheats.values():
        if normal_cost - cost >= 100:
            count += 1
    print(count)

solve()
