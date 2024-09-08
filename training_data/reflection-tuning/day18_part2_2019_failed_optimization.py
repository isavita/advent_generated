from collections import deque, defaultdict
import heapq

def parse_map(vault_map):
    grid = [list(row) for row in vault_map.split('\n')]
    keys = {}
    doors = {}
    robots = []
    
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell == '@':
                robots.append((x, y))
            elif cell.islower():
                keys[cell] = (x, y)
            elif cell.isupper():
                doors[cell.lower()] = (x, y)
    
    return grid, keys, doors, robots

def precompute_distances(grid, points):
    distances = defaultdict(dict)
    for start in points:
        queue = deque([(start, 0)])
        visited = set([start])
        while queue:
            (x, y), dist = queue.popleft()
            if grid[y][x] in points and (x, y) != start:
                distances[start][(x, y)] = dist
            for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
                nx, ny = x + dx, y + dy
                if 0 <= nx < len(grid[0]) and 0 <= ny < len(grid) and grid[ny][nx] != '#' and (nx, ny) not in visited:
                    queue.append(((nx, ny), dist + 1))
                    visited.add((nx, ny))
    return distances

def collect_keys(vault_map):
    grid, keys, doors, robots = parse_map(vault_map)
    all_points = list(keys.values()) + robots
    distances = precompute_distances(grid, all_points)
    
    initial_state = (tuple(robots), 0)  # (robot positions, collected keys bitmask)
    queue = [(0, initial_state)]
    visited = set()
    
    while queue:
        steps, (positions, collected) = heapq.heappop(queue)
        
        if collected == (1 << len(keys)) - 1:
            return steps
        
        state = (positions, collected)
        if state in visited:
            continue
        visited.add(state)
        
        for i, pos in enumerate(positions):
            for key, key_pos in keys.items():
                if collected & (1 << (ord(key) - ord('a'))):
                    continue
                if key_pos not in distances[pos]:
                    continue
                
                dist = distances[pos][key_pos]
                new_collected = collected | (1 << (ord(key) - ord('a')))
                new_positions = positions[:i] + (key_pos,) + positions[i+1:]
                new_state = (new_positions, new_collected)
                
                if new_state not in visited:
                    heapq.heappush(queue, (steps + dist, new_state))
    
    return -1

# Example usage
vault_map = """#######
#a.#Cd#
##@#@##
#######
##@#@##
#cB#Ab#
#######"""

print(collect_keys(vault_map))
