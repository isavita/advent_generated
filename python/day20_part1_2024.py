
from collections import deque

def solve():
    with open("input.txt", "r") as f:
        grid = [line.strip() for line in f]

    h, w = len(grid), len(grid[0])
    
    for r in range(h):
        for c in range(w):
            if grid[r][c] == 'S':
                S = (r, c)
            elif grid[r][c] == 'E':
                E = (r, c)
    
    track_cells = []
    walls = [[False] * w for _ in range(h)]
    for r in range(h):
        for c in range(w):
            if grid[r][c] == '#':
                walls[r][c] = True
            else:
                track_cells.append((r, c))

    dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

    def bfs(start, ignore_walls=False):
        dist = [[-1] * w for _ in range(h)]
        dist[start[0]][start[1]] = 0
        q = deque([start])
        while q:
            r, c = q.popleft()
            for dr, dc in dirs:
                nr, nc = r + dr, c + dc
                if 0 <= nr < h and 0 <= nc < w:
                    if not ignore_walls and walls[nr][nc]:
                        continue
                    if dist[nr][nc] == -1:
                        dist[nr][nc] = dist[r][c] + 1
                        q.append((nr, nc))
        return dist

    dist_from_s = bfs(S)
    dist_from_e = bfs(E)

    if dist_from_s[E[0]][E[1]] == -1:
        print(0)
        return

    normal_cost = dist_from_s[E[0]][E[1]]
    
    def is_track(r, c):
        return 0 <= r < h and 0 <= c < w and not walls[r][c]

    possible_cheats = 0
    for start_pos in track_cells:
        sd = dist_from_s[start_pos[0]][start_pos[1]]
        if sd == -1:
            continue
        
        for dr1, dc1 in dirs:
            m1r, m1c = start_pos[0] + dr1, start_pos[1] + dc1
            if not (0 <= m1r < h and 0 <= m1c < w):
                continue
            for dr2, dc2 in dirs:
                m2r, m2c = m1r + dr2, m1c + dc2
                if not (0 <= m2r < h and 0 <= m2c < w):
                    continue
                if not is_track(m2r, m2c):
                    continue
                ed = dist_from_e[m2r][m2c]
                if ed == -1:
                    continue
                new_cost = sd + 2 + ed
                saving = normal_cost - new_cost
                if saving >= 100:
                    possible_cheats += 1
    print(possible_cheats)

solve()
