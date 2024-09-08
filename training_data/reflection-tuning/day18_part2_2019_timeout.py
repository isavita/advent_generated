from collections import deque, defaultdict
import heapq

def parse_input(data):
    grid = [list(line.strip()) for line in data]
    keys, doors, starts = {}, {}, []
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell == '@':
                starts.append((x, y))
            elif cell.islower():
                keys[cell] = (x, y)
            elif cell.isupper():
                doors[cell.lower()] = (x, y)
    return grid, keys, doors, starts

def bfs(grid, start, keys, doors):
    queue = deque([(start, 0, 0)])
    visited = set()
    result = {}
    while queue:
        (x, y), dist, collected = queue.popleft()
        if (x, y, collected) in visited:
            continue
        visited.add((x, y, collected))
        cell = grid[y][x]
        if cell.islower() and cell not in result:
            result[cell] = (dist, collected)
        new_collected = collected
        if cell.islower():
            new_collected |= (1 << (ord(cell) - ord('a')))
        for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if 0 <= ny < len(grid) and 0 <= nx < len(grid[0]):
                next_cell = grid[ny][nx]
                if next_cell != '#' and (next_cell.isupper() and (collected & (1 << (ord(next_cell.lower()) - ord('a')))) or not next_cell.isupper()):
                    queue.append(((nx, ny), dist + 1, new_collected))
    return result

def solve(grid, keys, doors, starts):
    all_keys = (1 << len(keys)) - 1
    graph = defaultdict(dict)
    for start in starts:
        graph[start] = bfs(grid, start, keys, doors)
    for key, pos in keys.items():
        graph[key] = bfs(grid, pos, keys, doors)

    initial_state = tuple(starts) if len(starts) > 1 else starts[0]
    queue = [(0, initial_state, 0)]
    visited = set()

    while queue:
        dist, state, collected = heapq.heappop(queue)
        if collected == all_keys:
            return dist
        if (state, collected) in visited:
            continue
        visited.add((state, collected))

        if isinstance(state, tuple):  # Part 2: Multiple robots
            for i, pos in enumerate(state):
                for key, (key_dist, key_collected) in graph[pos].items():
                    if collected & (1 << (ord(key) - ord('a'))):
                        continue
                    new_collected = collected | (1 << (ord(key) - ord('a')))
                    new_state = state[:i] + (keys[key],) + state[i+1:]
                    heapq.heappush(queue, (dist + key_dist, new_state, new_collected))
        else:  # Part 1: Single robot
            for key, (key_dist, key_collected) in graph[state].items():
                if collected & (1 << (ord(key) - ord('a'))):
                    continue
                new_collected = collected | (1 << (ord(key) - ord('a')))
                heapq.heappush(queue, (dist + key_dist, keys[key], new_collected))

    return -1  # No solution found

def main():
    with open('input.txt', 'r') as f:
        data = f.readlines()

    grid, keys, doors, starts = parse_input(data)
    
    # Part 1
    result_part1 = solve(grid, keys, doors, starts)
    print(f"Part 1: {result_part1}")

    # Part 2
    center = starts[0]
    x, y = center
    for dx, dy in [(0, 0), (0, 1), (1, 0), (0, -1), (-1, 0)]:
        grid[y+dy][x+dx] = '#'
    new_starts = [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)]
    for sx, sy in new_starts:
        grid[sy][sx] = '@'
    
    result_part2 = solve(grid, keys, doors, new_starts)
    print(f"Part 2: {result_part2}")

if __name__ == "__main__":
    main()
