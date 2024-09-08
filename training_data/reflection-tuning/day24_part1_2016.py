from collections import deque
import itertools

def parse_input(filename):
    with open(filename, 'r') as file:
        grid = [list(line.strip()) for line in file]
    return grid

def find_points(grid):
    points = {}
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell.isdigit():
                points[int(cell)] = (x, y)
    return points

def bfs(grid, start, end):
    queue = deque([(start, 0)])
    visited = set([start])
    while queue:
        (x, y), dist = queue.popleft()
        if (x, y) == end:
            return dist
        for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if 0 <= nx < len(grid[0]) and 0 <= ny < len(grid) and grid[ny][nx] != '#' and (nx, ny) not in visited:
                queue.append(((nx, ny), dist + 1))
                visited.add((nx, ny))
    return float('inf')

def calculate_distances(grid, points):
    distances = {}
    for a, b in itertools.combinations(points.keys(), 2):
        dist = bfs(grid, points[a], points[b])
        distances[a, b] = distances[b, a] = dist
    return distances

def tsp_dp(distances, points):
    n = len(points)
    all_points = (1 << n) - 1
    dp = [[float('inf')] * n for _ in range(1 << n)]
    dp[1][0] = 0  # Start at point 0

    for mask in range(1, 1 << n):
        for end in range(1, n):  # Skip 0 as it's always the start
            if mask & (1 << end):
                for start in range(n):
                    if mask & (1 << start) and start != end:
                        dp[mask][end] = min(dp[mask][end],
                                            dp[mask ^ (1 << end)][start] + distances[start, end])

    return min(dp[all_points][end] for end in range(1, n))

def solve(filename):
    grid = parse_input(filename)
    points = find_points(grid)
    distances = calculate_distances(grid, points)
    return tsp_dp(distances, points)

print(solve('input.txt'))
