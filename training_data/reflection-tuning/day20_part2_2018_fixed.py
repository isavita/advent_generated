from collections import defaultdict

def parse_regex(regex):
    stack = []
    graph = defaultdict(set)
    x, y = 0, 0
    positions = [(0, 0)]

    for char in regex[1:-1]:  # Skip ^ and $
        if char == '(':
            stack.append(positions)
        elif char == ')':
            positions = stack.pop()
        elif char == '|':
            positions = stack[-1]
        else:
            new_positions = []
            for pos in positions:
                x, y = pos
                if char == 'N':
                    y -= 1
                elif char == 'S':
                    y += 1
                elif char == 'E':
                    x += 1
                elif char == 'W':
                    x -= 1
                graph[pos].add((x, y))
                graph[(x, y)].add(pos)
                new_positions.append((x, y))
            positions = new_positions

    return graph, (0, 0)

def bfs(graph, start):
    queue = [(start, 0)]
    visited = set([start])
    max_doors = 0
    rooms_over_1000 = 0

    while queue:
        pos, doors = queue.pop(0)
        max_doors = max(max_doors, doors)
        if doors >= 1000:
            rooms_over_1000 += 1

        for neighbor in graph[pos]:
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append((neighbor, doors + 1))

    return max_doors, rooms_over_1000

def solve(regex):
    graph, start = parse_regex(regex)
    return bfs(graph, start)

# Read input
with open('input.txt', 'r') as file:
    regex = file.read().strip()

part1, part2 = solve(regex)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
