from collections import deque, defaultdict

def parse_maze(lines):
    maze = [list(line.rstrip()) for line in lines]
    portals = defaultdict(list)
    start, end = None, None

    for y in range(len(maze)):
        for x in range(len(maze[y])):
            if maze[y][x].isalpha():
                # Check vertically first
                if y + 1 < len(maze) and maze[y+1][x].isalpha():
                    label = maze[y][x] + maze[y+1][x]
                    portal_y = y - 1 if y > 0 and maze[y-1][x] == '.' else y + 2
                    if 0 <= portal_y < len(maze) and maze[portal_y][x] == '.':
                        portals[label].append((x, portal_y))
                # Check horizontally if vertical check didn't work
                elif x + 1 < len(maze[y]) and maze[y][x+1].isalpha():
                    label = maze[y][x] + maze[y][x+1]
                    portal_x = x - 1 if x > 0 and maze[y][x-1] == '.' else x + 2
                    if 0 <= portal_x < len(maze[y]) and maze[y][portal_x] == '.':
                        portals[label].append((portal_x, y))

    start = portals['AA'][0]
    end = portals['ZZ'][0]
    del portals['AA'], portals['ZZ']

    return maze, dict(portals), start, end

def bfs(maze, portals, start, end):
    queue = deque([(start, 0)])
    visited = set([start])

    while queue:
        (x, y), steps = queue.popleft()

        if (x, y) == end:
            return steps

        for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if 0 <= ny < len(maze) and 0 <= nx < len(maze[ny]) and maze[ny][nx] == '.' and (nx, ny) not in visited:
                queue.append(((nx, ny), steps + 1))
                visited.add((nx, ny))

        # Check for portals
        for portal_positions in portals.values():
            if (x, y) in portal_positions:
                other_end = portal_positions[1] if portal_positions[0] == (x, y) else portal_positions[0]
                if other_end not in visited:
                    queue.append((other_end, steps + 1))
                    visited.add(other_end)

    return -1  # No path found

def solve_maze():
    with open('input.txt', 'r') as file:
        lines = file.readlines()
    
    maze, portals, start, end = parse_maze(lines)
    steps = bfs(maze, portals, start, end)
    print(f"Steps from AA to ZZ: {steps}")

solve_maze()
