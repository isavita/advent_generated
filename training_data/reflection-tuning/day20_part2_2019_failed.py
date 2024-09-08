from collections import deque, defaultdict

def parse_maze(lines):
    maze = {}
    portals = defaultdict(list)
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            if char != ' ':
                maze[(x, y)] = char
                if char.isupper():
                    portals[char].append((x, y))
    return maze, portals

def find_portal_pairs(maze, portals):
    portal_pairs = {}
    for portal, positions in portals.items():
        if len(positions) == 2:
            p1, p2 = positions
            for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
                if maze.get((p1[0] + dx, p1[1] + dy)) == '.':
                    portal_pairs[p1] = (p2, portal)
                    break
            for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
                if maze.get((p2[0] + dx, p2[1] + dy)) == '.':
                    portal_pairs[p2] = (p1, portal)
                    break
    return portal_pairs

def is_inner_portal(pos, maze):
    max_x = max(x for x, y in maze.keys())
    max_y = max(y for x, y in maze.keys())
    return 2 < pos[0] < max_x - 2 and 2 < pos[1] < max_y - 2

def bfs(maze, start, end, portal_pairs):
    queue = deque([(start, 0, 0)])  # (position, level, steps)
    visited = set()

    while queue:
        pos, level, steps = queue.popleft()

        if pos == end and level == 0:
            return steps

        if (pos, level) in visited:
            continue
        visited.add((pos, level))

        # Check neighboring tiles
        for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
            new_pos = (pos[0] + dx, pos[1] + dy)
            if maze.get(new_pos) == '.':
                queue.append((new_pos, level, steps + 1))

        # Check portals
        if pos in portal_pairs:
            other_pos, portal_name = portal_pairs[pos]
            if portal_name not in ('AA', 'ZZ'):
                new_level = level + (1 if is_inner_portal(pos, maze) else -1)
                if new_level >= 0:
                    for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
                        new_pos = (other_pos[0] + dx, other_pos[1] + dy)
                        if maze.get(new_pos) == '.':
                            queue.append((new_pos, new_level, steps + 1))
                            break

    return -1  # No path found

def main():
    with open('input.txt', 'r') as file:
        lines = file.read().splitlines()

    maze, portals = parse_maze(lines)
    portal_pairs = find_portal_pairs(maze, portals)

    start = next(pos for pos, (_, name) in portal_pairs.items() if name == 'AA')
    end = next(pos for pos, (_, name) in portal_pairs.items() if name == 'ZZ')

    steps = bfs(maze, start, end, portal_pairs)
    print(steps)

if __name__ == "__main__":
    main()
