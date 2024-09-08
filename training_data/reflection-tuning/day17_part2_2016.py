import hashlib
from collections import deque

def get_open_doors(passcode, path):
    hash_input = passcode + path
    hash_result = hashlib.md5(hash_input.encode()).hexdigest()[:4]
    return [c in 'bcdef' for c in hash_result]

def find_paths(passcode):
    queue = deque([((0, 0), '')])
    shortest_path = None
    longest_path_length = 0

    while queue:
        (x, y), path = queue.popleft()

        if (x, y) == (3, 3):
            if shortest_path is None:
                shortest_path = path
            longest_path_length = max(longest_path_length, len(path))
            continue

        open_doors = get_open_doors(passcode, path)
        moves = [('U', -1, 0), ('D', 1, 0), ('L', 0, -1), ('R', 0, 1)]

        for direction, dx, dy in moves:
            nx, ny = x + dx, y + dy
            if 0 <= nx < 4 and 0 <= ny < 4 and open_doors["UDLR".index(direction)]:
                queue.append(((nx, ny), path + direction))

    return shortest_path, longest_path_length

# Read input from file
with open('input.txt', 'r') as file:
    passcode = file.read().strip()

# Solve the puzzle
shortest_path, longest_path_length = find_paths(passcode)

# Print results
print(f"Shortest path: {shortest_path}")
print(f"Length of longest path: {longest_path_length}")
