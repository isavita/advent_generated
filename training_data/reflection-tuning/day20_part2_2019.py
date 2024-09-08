from collections import deque, defaultdict

def parse_maze(maze):
    portals = defaultdict(list)
    grid = {}
    for y, line in enumerate(maze.splitlines()):
        for x, char in enumerate(line):
            if char != ' ':
                grid[x, y] = char

    # Find portals
    for (x, y), char in grid.items():
        if char.isupper():
            for dx, dy in [(0, 1), (1, 0)]:
                if (x+dx, y+dy) in grid and grid[x+dx, y+dy].isupper():
                    label = char + grid[x+dx, y+dy]
                    if (x-1, y) in grid and grid[x-1, y] == '.':
                        portals[label].append((x-1, y))
                    elif (x+2, y) in grid and grid[x+2, y] == '.':
                        portals[label].append((x+2, y))
                    elif (x, y-1) in grid and grid[x, y-1] == '.':
                        portals[label].append((x, y-1))
                    elif (x, y+2) in grid and grid[x, y+2] == '.':
                        portals[label].append((x, y+2))

    return grid, portals

def is_outer(pos, grid):
    x, y = pos
    return x == 2 or y == 2 or x == max(p[0] for p in grid) - 2 or y == max(p[1] for p in grid) - 2

def bfs(grid, portals, recursive=False):
    start = portals['AA'][0]
    end = portals['ZZ'][0]
    
    queue = deque([(start, 0, 0)])  # (position, steps, level)
    visited = set([(start, 0)])

    while queue:
        pos, steps, level = queue.popleft()

        if pos == end and level == 0:
            return steps

        for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
            new_pos = (pos[0] + dx, pos[1] + dy)
            if new_pos in grid and grid[new_pos] == '.' and (new_pos, level) not in visited:
                queue.append((new_pos, steps + 1, level))
                visited.add((new_pos, level))

        # Handle portals
        for label, positions in portals.items():
            if pos in positions and label not in ['AA', 'ZZ']:
                new_pos = positions[1] if pos == positions[0] else positions[0]
                new_level = level
                if recursive:
                    if is_outer(pos, grid):
                        new_level -= 1
                    else:
                        new_level += 1
                    if new_level < 0:
                        continue
                if (new_pos, new_level) not in visited:
                    queue.append((new_pos, steps + 1, new_level))
                    visited.add((new_pos, new_level))

    return -1  # No path found

def main():
    with open('input.txt', 'r') as f:
        maze = f.read()

    grid, portals = parse_maze(maze)

    # Part 1
    steps = bfs(grid, portals)
    print(f"Part 1: {steps}")

    # Part 2
    steps_recursive = bfs(grid, portals, recursive=True)
    print(f"Part 2: {steps_recursive}")

if __name__ == "__main__":
    main()
