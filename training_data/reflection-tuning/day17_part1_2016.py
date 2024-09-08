import hashlib
from collections import deque

def find_shortest_path(passcode):
    queue = deque([((0, 0), "")])
    directions = [('U', -1, 0), ('D', 1, 0), ('L', 0, -1), ('R', 0, 1)]

    while queue:
        (x, y), path = queue.popleft()
        
        if (x, y) == (3, 3):
            return path
        
        hash_input = passcode + path
        hash_result = hashlib.md5(hash_input.encode()).hexdigest()[:4]
        
        for i, (direction, dx, dy) in enumerate(directions):
            nx, ny = x + dx, y + dy
            if 0 <= nx < 4 and 0 <= ny < 4 and hash_result[i] in 'bcdef':
                queue.append(((nx, ny), path + direction))

    return "No path found"

# Read input from file
with open("input.txt", "r") as file:
    passcode = file.read().strip()

# Find and print the shortest path
shortest_path = find_shortest_path(passcode)
print(shortest_path)
