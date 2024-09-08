from collections import deque
import math

def lcm(a, b):
    return abs(a * b) // math.gcd(a, b)

def parse_input(input_str):
    lines = input_str.strip().split('\n')
    height, width = len(lines), len(lines[0])
    blizzards = []
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            if char in '^v<>':
                blizzards.append((x, y, char))
    return blizzards, (width, height)

def move_blizzard(x, y, direction, width, height):
    if direction == '^':
        return (x, (y - 2) % (height - 2) + 1)
    elif direction == 'v':
        return (x, y % (height - 2) + 1)
    elif direction == '<':
        return ((x - 2) % (width - 2) + 1, y)
    elif direction == '>':
        return (x % (width - 2) + 1, y)

def calculate_blizzard_positions(blizzards, dimensions):
    width, height = dimensions
    cycle_length = lcm(width - 2, height - 2)
    blizzard_positions = [set() for _ in range(cycle_length)]
    
    for t in range(cycle_length):
        for x, y, direction in blizzards:
            new_x, new_y = move_blizzard(x, y, direction, width, height)
            blizzard_positions[t].add((new_x, new_y))
        blizzards = [(new_x, new_y, direction) for (x, y, direction) in blizzards]
    
    return blizzard_positions

def find_shortest_path(start, goal, blizzard_positions, dimensions):
    width, height = dimensions
    queue = deque([(start, 0)])
    visited = set()
    
    while queue:
        (x, y), time = queue.popleft()
        
        if (x, y) == goal:
            return time
        
        next_time = (time + 1) % len(blizzard_positions)
        for dx, dy in [(0, 0), (0, 1), (1, 0), (0, -1), (-1, 0)]:
            nx, ny = x + dx, y + dy
            if (nx, ny) == goal or (nx, ny) == start:
                queue.append(((nx, ny), time + 1))
            elif 0 < nx < width - 1 and 0 < ny < height - 1 and (nx, ny) not in blizzard_positions[next_time]:
                state = (nx, ny, next_time)
                if state not in visited:
                    visited.add(state)
                    queue.append(((nx, ny), time + 1))
    
    return -1  # No path found

def solve(input_str):
    blizzards, dimensions = parse_input(input_str)
    width, height = dimensions
    start = (1, 0)
    goal = (width - 2, height - 1)
    
    blizzard_positions = calculate_blizzard_positions(blizzards, dimensions)
    return find_shortest_path(start, goal, blizzard_positions, dimensions)

# Example usage:
input_str = """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"""

result = solve(input_str)
print(f"The fewest number of minutes required: {result}")
