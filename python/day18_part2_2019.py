import sys
from collections import deque, defaultdict
import heapq

# Read the map from 'input.txt'
with open('input.txt') as f:
    original_map = [list(line.strip('\n')) for line in f if line.strip('\n')]

# Modify the map as per Part Two instructions
# Find the position of '@' surrounded by open spaces ('.')

found = False
for y in range(1, len(original_map) - 1):
    for x in range(1, len(original_map[0]) - 1):
        if original_map[y][x] == '@':
            # Check if the surrounding positions are open space ('.')
            if (original_map[y-1][x] == '.' and original_map[y+1][x] == '.' and
                original_map[y][x-1] == '.' and original_map[y][x+1] == '.'):
                # Modify the map
                original_map[y-1][x-1:x+2] = ['@', '#', '@']
                original_map[y][x-1:x+2] = ['#', '#', '#']
                original_map[y+1][x-1:x+2] = ['@', '#', '@']
                found = True
                break
    if found:
        break

if not found:
    print("Error: Could not find the '@' symbol surrounded by open spaces.")
    sys.exit(1)

# Now, find the positions of the 4 robots
robot_positions = []
for y, row in enumerate(original_map):
    for x, cell in enumerate(row):
        if cell == '@':
            robot_positions.append((x, y))

# Collect all keys and doors
keys = {}
doors = {}
all_keys = set()
for y, row in enumerate(original_map):
    for x, cell in enumerate(row):
        if cell.islower():
            keys[cell] = (x, y)
            all_keys.add(cell)
        elif cell.isupper():
            doors[cell] = (x, y)

# Precompute reachable keys and distances for each robot
def bfs(start_pos):
    queue = deque()
    queue.append((start_pos[0], start_pos[1], 0, frozenset()))
    visited = set()
    results = {}

    while queue:
        x, y, dist, required_keys = queue.popleft()
        if (x, y) in visited:
            continue
        visited.add((x, y))

        cell = original_map[y][x]
        if cell.islower() and cell not in required_keys:
            # Found a key
            results[cell] = (dist, required_keys)
            required_keys = required_keys | {cell}

        for dx, dy in [(-1,0), (1,0), (0,-1), (0,1)]:
            nx, ny = x + dx, y + dy
            if 0 <= ny < len(original_map) and 0 <= nx < len(original_map[0]):
                ncell = original_map[ny][nx]
                if ncell != '#':
                    if ncell.isupper():
                        # Door, need the key
                        n_required_keys = required_keys | {ncell.lower()}
                        queue.append((nx, ny, dist+1, n_required_keys))
                    else:
                        queue.append((nx, ny, dist+1, required_keys))
    return results

# For each robot, compute reachable keys
robot_reachable_keys = []
for pos in robot_positions:
    reachable = bfs(pos)
    robot_reachable_keys.append(reachable)

# Precompute distances between keys
key_positions = {**keys}
for i, pos in enumerate(robot_positions):
    key_positions[f'@{i}'] = pos

def key_bfs(start_key):
    start_pos = key_positions[start_key]
    queue = deque()
    queue.append((start_pos[0], start_pos[1], 0, frozenset()))
    visited = set()
    results = {}

    while queue:
        x, y, dist, required_keys = queue.popleft()
        if (x, y) in visited:
            continue
        visited.add((x, y))

        cell = original_map[y][x]
        if cell.islower() and cell != start_key.lower() and cell not in required_keys:
            # Found a key
            results[cell] = (dist, required_keys)
            required_keys = required_keys | {cell}

        for dx, dy in [(-1,0), (1,0), (0,-1), (0,1)]:
            nx, ny = x + dx, y + dy
            if 0 <= ny < len(original_map) and 0 <= nx < len(original_map[0]):
                ncell = original_map[ny][nx]
                if ncell != '#':
                    if ncell.isupper():
                        # Door, need the key
                        n_required_keys = required_keys | {ncell.lower()}
                        queue.append((nx, ny, dist+1, required_keys))
                    else:
                        queue.append((nx, ny, dist+1, required_keys))
    return results

# Build a graph of keys and distances
key_graph = {}
all_nodes = list(keys.keys()) + [f'@{i}' for i in range(len(robot_positions))]
for key in all_nodes:
    key_graph[key] = key_bfs(key)

# Now, perform BFS (Dijkstra) over the state space
from collections import namedtuple

import functools

def bitmask(keys_set):
    mask = 0
    for k in keys_set:
        mask |= 1 << (ord(k) - ord('a'))
    return mask

def dijkstra():
    total_keys = len(all_keys)
    initial_positions = tuple(f'@{i}' for i in range(len(robot_positions)))
    initial_state = (initial_positions, frozenset())
    heap = [(0, initial_positions, frozenset())]
    visited = {}
    while heap:
        cost, positions, collected_keys = heapq.heappop(heap)
        state = (positions, bitmask(collected_keys))
        if state in visited and visited[state] <= cost:
            continue
        visited[state] = cost
        if len(collected_keys) == total_keys:
            return cost
        # For each robot, try to move to new keys
        for i in range(len(positions)):
            pos = positions[i]
            available_keys = []
            for key, (dist, required_keys) in key_graph[pos].items():
                if key not in collected_keys and required_keys.issubset(collected_keys):
                    available_keys.append((key, dist))
            for key, dist in available_keys:
                new_positions = list(positions)
                new_positions[i] = key
                new_positions = tuple(new_positions)
                new_collected_keys = collected_keys | {key}
                new_state = (new_positions, bitmask(new_collected_keys))
                if new_state in visited and visited[new_state] <= cost + dist:
                    continue
                heapq.heappush(heap, (cost + dist, new_positions, new_collected_keys))
    return None

result = dijkstra()
print(result)
