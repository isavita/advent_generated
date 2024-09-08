from collections import deque, defaultdict
import heapq

def shortest_path(grid):
    rows, cols = len(grid), len(grid[0])
    keys = {}
    doors = {}
    start = None

    # Find keys, doors, and start position
    for i in range(rows):
        for j in range(cols):
            if grid[i][j] == '@':
                start = (i, j)
            elif grid[i][j].islower():
                keys[grid[i][j]] = (i, j)
            elif grid[i][j].isupper():
                doors[grid[i][j]] = (i, j)

    all_keys = set(keys.keys())
    key_bits = {k: 1 << i for i, k in enumerate(all_keys)}

    # Precompute distances between keys and start
    distances = defaultdict(dict)
    for start_pos in [start] + list(keys.values()):
        queue = deque([(start_pos, 0)])
        visited = set()
        while queue:
            pos, dist = queue.popleft()
            if pos in visited:
                continue
            visited.add(pos)
            x, y = pos
            char = grid[x][y]
            if char.islower() and pos != start_pos:
                distances[start_pos][char] = (dist, pos)
            for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
                nx, ny = x + dx, y + dy
                if 0 <= nx < rows and 0 <= ny < cols and grid[nx][ny] != '#':
                    queue.append(((nx, ny), dist + 1))

    # BFS with priority queue
    initial_state = (0, start, 0)  # (steps, position, collected_keys)
    queue = [initial_state]
    visited = set()

    while queue:
        steps, pos, collected = heapq.heappop(queue)
        if collected == (1 << len(all_keys)) - 1:
            return steps
        
        state = (pos, collected)
        if state in visited:
            continue
        visited.add(state)

        for key, (dist, key_pos) in distances[pos].items():
            if key_bits[key] & collected:
                continue
            new_collected = collected | key_bits[key]
            heapq.heappush(queue, (steps + dist, key_pos, new_collected))

    return -1  # No solution found

# Example usage:
grid = [
    "########################",
    "#f.D.E.e.C.b.A.@.a.B.c.#",
    "######################.#",
    "#d.....................#",
    "########################"
]

print(shortest_path(grid))
