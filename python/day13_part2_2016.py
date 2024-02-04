
from collections import deque

favorite_number = int(open("input.txt", "r").read())

def is_valid_move(x, y):
    if x < 0 or y < 0:
        return False
    num = x*x + 3*x + 2*x*y + y + y*y + favorite_number
    return bin(num).count('1') % 2 == 0

def count_reachable_locations(target_steps):
    visited = set()
    queue = deque([(1, 1, 0)])

    while queue:
        x, y, steps = queue.popleft()

        if steps > target_steps:
            continue

        if (x, y) in visited:
            continue

        visited.add((x, y))

        if steps == target_steps:
            continue

        for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            new_x, new_y = x + dx, y + dy
            if is_valid_move(new_x, new_y):
                queue.append((new_x, new_y, steps + 1))

    return len(visited)

print(count_reachable_locations(50))
