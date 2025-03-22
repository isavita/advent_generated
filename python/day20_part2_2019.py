from collections import deque, defaultdict

def read_input(filename="input.txt"):
    with open(filename) as f:
        return [line.rstrip("\n") for line in f.readlines()]

def find_portals(grid):
    portals = defaultdict(list)
    height = len(grid)
    width = max(len(row) for row in grid)
    
    # pad grid to handle out-of-bound checks easily
    grid = [row.ljust(width) for row in grid]
    
    # helper to check if a character is uppercase letter
    def is_letter(c):
        return 'A' <= c <= 'Z'
    
    for y in range(height):
        for x in range(width):
            # check horizontal portal labels
            if x < width - 1 and is_letter(grid[y][x]) and is_letter(grid[y][x+1]):
                label = grid[y][x] + grid[y][x+1]
                # determine portal position (the '.' adjacent)
                pos = None
                # left side check
                if x - 1 >= 0 and grid[y][x-1] == '.':
                    pos = (x-1, y)
                # right side check
                elif x + 2 < width and grid[y][x+2] == '.':
                    pos = (x+2, y)
                if pos:
                    # determine outer vs inner later based on pos location
                    portals[label].append(pos)
            # check vertical portal labels
            if y < height - 1 and is_letter(grid[y][x]) and is_letter(grid[y+1][x]):
                label = grid[y][x] + grid[y+1][x]
                pos = None
                # above check
                if y - 1 >= 0 and grid[y-1][x] == '.':
                    pos = (x, y-1)
                # below check
                elif y + 2 < height and grid[y+2][x] == '.':
                    pos = (x, y+2)
                if pos:
                    portals[label].append(pos)
    return portals, width, height

def is_outer(pos, width, height):
    x, y = pos
    # using a threshold of 3 based on typical maze structure
    return x <= 2 or y <= 2 or x >= width - 3 or y >= height - 3

def build_portal_mapping(portals, width, height):
    portal_map = {}
    start = end = None
    for label, positions in portals.items():
        if label == "AA":
            start = positions[0]
        elif label == "ZZ":
            end = positions[0]
        elif len(positions) == 2:
            a, b = positions
            # Determine if positions are outer or inner
            a_outer = is_outer(a, width, height)
            b_outer = is_outer(b, width, height)
            portal_map[a] = (b, a_outer)
            portal_map[b] = (a, b_outer)
    return start, end, portal_map

def bfs_recursive(grid, start, end, portal_map, width, height):
    # state: (x, y, level)
    queue = deque()
    queue.append((start[0], start[1], 0, 0))  # (x, y, level, steps)
    visited = set()
    visited.add((start[0], start[1], 0))
    # movement directions
    directions = [(0,1), (0,-1), (1,0), (-1,0)]
    
    while queue:
        x, y, level, steps = queue.popleft()
        # reached end at outermost level?
        if (x, y) == end and level == 0:
            return steps
        # try moving in four directions
        for dx, dy in directions:
            nx, ny = x+dx, y+dy
            if ny < 0 or ny >= len(grid) or nx < 0 or nx >= len(grid[ny]):
                continue
            if grid[ny][nx] != '.':
                continue
            state = (nx, ny, level)
            if state not in visited:
                visited.add(state)
                queue.append((nx, ny, level, steps+1))
        # check if on a portal tile
        if (x, y) in portal_map:
            target, outer = portal_map[(x, y)]
            # Only use portal if not at AA/ZZ
            # Determine new level: if outer then level - 1, if inner then level + 1
            new_level = level - 1 if outer else level + 1
            # Cannot go to negative level
            if new_level < 0:
                continue
            tx, ty = target
            state = (tx, ty, new_level)
            if state not in visited:
                visited.add(state)
                queue.append((tx, ty, new_level, steps+1))
    return -1  # no solution found

def main():
    grid = read_input("input.txt")
    portals, width, height = find_portals(grid)
    start, end, portal_map = build_portal_mapping(portals, width, height)
    result = bfs_recursive(grid, start, end, portal_map, width, height)
    print(result)

if __name__ == "__main__":
    main()
