from collections import deque
import math

def parse_input(input_data):
    grid = [list(line.strip()) for line in input_data.split('\n') if line.strip()]
    blizzards = []
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell in '^v<>':
                blizzards.append((x, y, cell))
                grid[y][x] = '.'
    return grid, blizzards

def move_blizzard(x, y, direction, width, height):
    if direction == '^':
        return (x, (y - 2) % (height - 2) + 1)
    elif direction == 'v':
        return (x, y % (height - 2) + 1)
    elif direction == '<':
        return ((x - 2) % (width - 2) + 1, y)
    elif direction == '>':
        return (x % (width - 2) + 1, y)

def bfs(grid, blizzards, start, goal, time=0):
    width, height = len(grid[0]), len(grid)
    lcm = math.lcm(width - 2, height - 2)
    queue = deque([(start[0], start[1], time)])
    visited = set()

    while queue:
        x, y, t = queue.popleft()
        if (x, y) == goal:
            return t, blizzards

        if (x, y, t % lcm) in visited:
            continue
        visited.add((x, y, t % lcm))

        next_blizzards = set()
        for bx, by, direction in blizzards:
            next_blizzards.add(move_blizzard(bx, by, direction, width, height))
        blizzards = list((x, y, grid[y][x]) for x, y in next_blizzards)

        for dx, dy in [(0, 0), (0, 1), (1, 0), (0, -1), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if 0 <= nx < width and 0 <= ny < height and grid[ny][nx] != '#' and (nx, ny) not in next_blizzards:
                queue.append((nx, ny, t + 1))

    return None

def solve(input_data):
    grid, blizzards = parse_input(input_data)
    start = (1, 0)
    goal = (len(grid[0]) - 2, len(grid) - 1)

    # Part 1
    time1, blizzards = bfs(grid, blizzards, start, goal)
    print(f"Part 1: {time1}")

    # Part 2
    time2, blizzards = bfs(grid, blizzards, goal, start, time1)
    time3, blizzards = bfs(grid, blizzards, start, goal, time2)
    total_time = time3
    print(f"Part 2: {total_time}")

# Example usage:
input_data = """
#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
"""
solve(input_data)
