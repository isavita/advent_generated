from collections import defaultdict, deque
import sys

def read_maze(filename):
    with open(filename, 'r') as file:
        maze = [list(line.strip('\n')) for line in file]
    return maze

def find_portals(maze):
    height = len(maze)
    width = len(maze[0])
    portals = defaultdict(list)
    portal_positions = {}
    
    for y in range(height):
        for x in range(width):
            if 'A' <= maze[y][x] <= 'Z':
                # Check right and down for possible portal
                if x + 1 < width and 'A' <= maze[y][x+1] <= 'Z':
                    portal_name = maze[y][x] + maze[y][x+1]
                    if x + 2 < width and maze[y][x+2] == '.':
                        portals[portal_name].append((x+2, y))
                        portal_positions[(x+2, y)] = portal_name
                    elif x - 1 >= 0 and maze[y][x-1] == '.':
                        portals[portal_name].append((x-1, y))
                        portal_positions[(x-1, y)] = portal_name
                if y + 1 < height and 'A' <= maze[y+1][x] <= 'Z':
                    portal_name = maze[y][x] + maze[y+1][x]
                    if y + 2 < height and maze[y+2][x] == '.':
                        portals[portal_name].append((x, y+2))
                        portal_positions[(x, y+2)] = portal_name
                    elif y - 1 >= 0 and maze[y-1][x] == '.':
                        portals[portal_name].append((x, y-1))
                        portal_positions[(x, y-1)] = portal_name

    return portals, portal_positions

def bfs(maze, portals, portal_positions, start, end):
    queue = deque([(start[0], start[1], 0)])  # (x, y, steps)
    visited = set()
    visited.add(start)
    
    while queue:
        x, y, steps = queue.popleft()
        
        for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nx, ny = x + dx, y + dy
            if 0 <= nx < len(maze[0]) and 0 <= ny < len(maze) and maze[ny][nx] == '.':
                if (nx, ny) == end:
                    return steps + 1
                if (nx, ny) not in visited:
                    visited.add((nx, ny))
                    queue.append((nx, ny, steps + 1))
        
        if (x, y) in portal_positions:
            portal_name = portal_positions[(x, y)]
            for px, py in portals[portal_name]:
                if (px, py) != (x, y) and (px, py) not in visited:
                    visited.add((px, py))
                    queue.append((px, py, steps + 1))
    
    return -1

def main():
    maze = read_maze("input.txt")
    portals, portal_positions = find_portals(maze)

    start = portals['AA'][0]
    end = portals['ZZ'][0]
    
    result = bfs(maze, portals, portal_positions, start, end)
    print(result)

if __name__ == "__main__":
    main()