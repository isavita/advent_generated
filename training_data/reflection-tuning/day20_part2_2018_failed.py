from collections import deque, defaultdict

def parse_regex(regex):
    stack = [(0, 0)]
    positions = {(0, 0)}
    max_x = max_y = min_x = min_y = 0
    graph = defaultdict(set)

    for char in regex[1:-1]:  # Skip ^ and $
        if char in 'NESW':
            dx, dy = {'N': (0, -1), 'E': (1, 0), 'S': (0, 1), 'W': (-1, 0)}[char]
            new_positions = set()
            for x, y in positions:
                new_x, new_y = x + dx, y + dy
                graph[(x, y)].add((new_x, new_y))
                graph[(new_x, new_y)].add((x, y))
                new_positions.add((new_x, new_y))
                max_x = max(max_x, new_x)
                max_y = max(max_y, new_y)
                min_x = min(min_x, new_x)
                min_y = min(min_y, new_y)
            positions = new_positions
        elif char == '(':
            stack.append(list(positions))
        elif char == ')':
            positions = set(stack.pop())
        elif char == '|':
            stack[-1].extend(positions)
            positions = set(stack[-2])

    return graph, (min_x, min_y, max_x, max_y)

def bfs(graph, start):
    queue = deque([(start, 0)])
    distances = {start: 0}
    max_distance = 0
    rooms_over_1000 = 0

    while queue:
        pos, dist = queue.popleft()
        for neighbor in graph[pos]:
            if neighbor not in distances:
                new_dist = dist + 1
                distances[neighbor] = new_dist
                queue.append((neighbor, new_dist))
                max_distance = max(max_distance, new_dist)
                if new_dist >= 1000:
                    rooms_over_1000 += 1

    return max_distance, rooms_over_1000

def solve(regex):
    graph, _ = parse_regex(regex)
    return bfs(graph, (0, 0))

# Read input from file
with open('input.txt', 'r') as file:
    regex = file.read().strip()

part1, part2 = solve(regex)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
