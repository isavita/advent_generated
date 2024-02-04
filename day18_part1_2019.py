
from collections import deque

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

class State:
    def __init__(self, pos, keys):
        self.pos = pos
        self.keys = keys

    def __hash__(self):
        return hash((self.pos.x, self.pos.y, self.keys))

    def __eq__(self, other):
        return self.pos.x == other.pos.x and self.pos.y == other.pos.y and self.keys == other.keys

def find_shortest_path(grid, start, key_map):
    dirs = [Point(0, -1), Point(-1, 0), Point(0, 1), Point(1, 0)]
    visited = set()
    queue = deque([State(start, 0)])
    steps = 0

    while queue:
        size = len(queue)
        for _ in range(size):
            current = queue.popleft()

            if current.keys == (1 << len(key_map)) - 1:
                return steps

            for d in dirs:
                next_pos = Point(current.pos.x + d.x, current.pos.y + d.y)
                if 0 <= next_pos.x < len(grid[0]) and 0 <= next_pos.y < len(grid):
                    char = grid[next_pos.y][next_pos.x]
                    if char != '#' and not (char.isupper() and not current.keys & (1 << key_map[chr(ord(char) + 32)])):
                        new_state = State(next_pos, current.keys)
                        if char.islower():
                            new_state.keys |= 1 << key_map[char]
                        if new_state not in visited:
                            visited.add(new_state)
                            queue.append(new_state)
        steps += 1
    return -1

def main():
    with open("input.txt", "r") as file:
        grid = []
        start = None
        key_map = {}
        key_counter = 0

        for y, line in enumerate(file):
            line = line.strip()
            grid.append(line)
            for x, char in enumerate(line):
                if char == '@':
                    start = Point(x, y)
                elif char.islower():
                    key_map[char] = key_counter
                    key_counter += 1

    print(find_shortest_path(grid, start, key_map))

if __name__ == "__main__":
    main()
