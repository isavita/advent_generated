
import sys

def solve():
    with open("input.txt") as f:
        lines = f.read().splitlines()

    nr = len(lines)
    nc = len(lines[0])
    grid = [[int(c) for c in line] for line in lines]
    dp = [([-1] * nc) for _ in range(nr)]
    dirs = [(1,0),(-1,0),(0,1),(0,-1)]

    def dfs(r, c):
        if dp[r][c] != -1:
            return dp[r][c]
        h = grid[r][c]
        if h == 9:
            dp[r][c] = 1
            return 1
        sum_paths = 0
        for dr, dc in dirs:
            nr2, nc2 = r + dr, c + dc
            if 0 <= nr2 < nr and 0 <= nc2 < nc and grid[nr2][nc2] == h + 1:
                sum_paths += dfs(nr2, nc2)
        dp[r][c] = sum_paths
        return sum_paths

    total = 0
    for r in range(nr):
        for c in range(nc):
            if grid[r][c] == 0:
                total += dfs(r, c)
    print(total)

solve()
