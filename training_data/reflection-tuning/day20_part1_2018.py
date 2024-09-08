from collections import defaultdict, deque

def parse_regex(regex):
    graph = defaultdict(set)
    stack = [(0, 0)]
    pos = (0, 0)
    
    for char in regex[1:-1]:  # Skip ^ and $
        if char == '(':
            stack.append(pos)
        elif char == ')':
            stack.pop()
        elif char == '|':
            pos = stack[-1]
        else:
            new_pos = pos
            if char == 'N':
                new_pos = (pos[0], pos[1] + 1)
            elif char == 'S':
                new_pos = (pos[0], pos[1] - 1)
            elif char == 'E':
                new_pos = (pos[0] + 1, pos[1])
            elif char == 'W':
                new_pos = (pos[0] - 1, pos[1])
            
            graph[pos].add(new_pos)
            graph[new_pos].add(pos)
            pos = new_pos
    
    return graph

def find_furthest_room(graph):
    queue = deque([(0, 0, 0)])  # (x, y, distance)
    visited = set()
    max_distance = 0

    while queue:
        x, y, distance = queue.popleft()
        if (x, y) in visited:
            continue
        
        visited.add((x, y))
        max_distance = max(max_distance, distance)

        for nx, ny in graph[(x, y)]:
            if (nx, ny) not in visited:
                queue.append((nx, ny, distance + 1))

    return max_distance

# Read input from file
with open('input.txt', 'r') as file:
    regex = file.read().strip()

# Solve the problem
graph = parse_regex(regex)
result = find_furthest_room(graph)

# Print the result
print(result)
