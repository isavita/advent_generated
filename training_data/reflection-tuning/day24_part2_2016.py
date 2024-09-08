from collections import deque
import itertools

def parse_input(filename):
    with open(filename, 'r') as f:
        grid = [list(line.strip()) for line in f]
    points = {}
    for i, row in enumerate(grid):
        for j, cell in enumerate(row):
            if cell.isdigit():
                points[int(cell)] = (i, j)
    return grid, points

def bfs(grid, start, end):
    queue = deque([(start, 0)])
    visited = set([start])
    while queue:
        (x, y), dist = queue.popleft()
        if (x, y) == end:
            return dist
        for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if 0 <= nx < len(grid) and 0 <= ny < len(grid[0]) and grid[nx][ny] != '#' and (nx, ny) not in visited:
                queue.append(((nx, ny), dist + 1))
                visited.add((nx, ny))
    return float('inf')

def solve(grid, points):
    distances = {(i, j): bfs(grid, points[i], points[j]) for i in points for j in points if i != j}
    
    non_zero_points = [p for p in points if p != 0]
    min_distance = float('inf')
    min_distance_with_return = float('inf')

    for perm in itertools.permutations(non_zero_points):
        route = [0] + list(perm)
        distance = sum(distances[(route[i], route[i+1])] for i in range(len(route)-1))
        min_distance = min(min_distance, distance)
        min_distance_with_return = min(min_distance_with_return, distance + distances[(route[-1], 0)])

    return min_distance, min_distance_with_return

grid, points = parse_input("input.txt")
part1, part2 = solve(grid, points)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
