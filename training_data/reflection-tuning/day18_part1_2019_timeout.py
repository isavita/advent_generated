from collections import deque, defaultdict
import heapq

def parse_input(lines):
    grid = [list(line.strip()) for line in lines]
    keys = {}
    start = None
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell == '@':
                start = (x, y)
            elif cell.islower():
                keys[cell] = (x, y)
    return grid, start, keys

def bfs(grid, start, end, keys):
    queue = deque([(start, 0)])
    visited = set([start])
    while queue:
        (x, y), dist = queue.popleft()
        if (x, y) == end:
            return dist
        for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if (nx, ny) not in visited and grid[ny][nx] != '#':
                if grid[ny][nx].isupper() and grid[ny][nx].lower() not in keys:
                    continue
                visited.add((nx, ny))
                queue.append(((nx, ny), dist + 1))
    return float('inf')

def find_shortest_path(grid, start, keys):
    key_positions = {k: v for k, v in keys.items()}
    key_positions['@'] = start
    
    distances = defaultdict(dict)
    for k1 in key_positions:
        for k2 in key_positions:
            if k1 != k2:
                dist = bfs(grid, key_positions[k1], key_positions[k2], set(key_positions.keys()))
                distances[k1][k2] = distances[k2][k1] = dist
    
    target_keys = (1 << len(keys)) - 1
    queue = [(0, '@', 0)]
    visited = {}
    
    while queue:
        dist, pos, collected = heapq.heappop(queue)
        if collected == target_keys:
            return dist
        
        if (pos, collected) in visited and visited[(pos, collected)] <= dist:
            continue
        visited[(pos, collected)] = dist
        
        for key, key_dist in distances[pos].items():
            if key.islower():
                key_bit = ord(key) - ord('a')
                if collected & (1 << key_bit):
                    continue
                new_collected = collected | (1 << key_bit)
                heapq.heappush(queue, (dist + key_dist, key, new_collected))

def solve(input_file):
    with open(input_file, 'r') as f:
        lines = f.readlines()
    
    grid, start, keys = parse_input(lines)
    return find_shortest_path(grid, start, keys)

print(solve('input.txt'))
