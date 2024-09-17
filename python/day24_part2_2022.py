import math
from collections import deque

def read_input(file_path):
    walls = set()
    blizzards = []
    with open(file_path, 'r') as file:
        for y, line in enumerate(file):
            line = line.strip()
            for x, char in enumerate(line):
                if char == '#':
                    walls.add((x, y))
                elif char in {'>', '<', '^', 'v'}:
                    blizzards.append((x, y, char))
    height = y + 1
    width = x + 1
    return walls, blizzards, height, width

def find_start_end(walls, height, width):
    # Start is the only open cell in the top row
    for x in range(width):
        if (x, 0) not in walls:
            start = (x, 0)
            break
    # End is the only open cell in the bottom row
    for x in range(width):
        if (x, height -1) not in walls:
            end = (x, height -1)
            break
    return start, end

def lcm(a, b):
    return abs(a*b) // math.gcd(a, b)

def compute_period(width, height):
    return lcm(width, height)

def precompute_blizzards(blizzards, width, height, period):
    blizzard_positions = [set() for _ in range(period)]
    for t in range(period):
        for b in blizzards:
            x, y, dir = b
            if dir == '>':
                new_x = 1 + ((x -1 + t) % (width -2))
                new_y = y
            elif dir == '<':
                new_x = 1 + ((x -1 - t) % (width -2))
                new_y = y
            elif dir == 'v':
                new_x = x
                new_y = 1 + ((y -1 + t) % (height -2))
            elif dir == '^':
                new_x = x
                new_y = 1 + ((y -1 - t) % (height -2))
            blizzard_positions[t].add((new_x, new_y))
    return blizzard_positions

def bfs(start, end, walls, blizzard_positions, period, width, height, start_time):
    queue = deque()
    visited = set()
    queue.append((start[0], start[1], start_time))  # x, y, time
    visited.add((start[0], start[1], start_time % period))
    
    directions = [ (0,0), (1,0), (-1,0), (0,1), (0,-1) ]  # Wait, Right, Left, Down, Up
    
    while queue:
        x, y, t = queue.popleft()
        # If reached the end
        if (x, y) == end:
            return t
        next_t = t + 1
        blizzards_next = blizzard_positions[next_t % period]
        for dx, dy in directions:
            nx, ny = x + dx, y + dy
            # Check if the new position is the end
            if (nx, ny) == end:
                return next_t
            # Check if the new position is the start
            if (nx, ny) == start:
                state = (nx, ny, next_t % period)
                if state not in visited and (nx, ny) not in blizzards_next:
                    visited.add(state)
                    queue.append((nx, ny, next_t))
                continue
            # Check boundaries
            if 1 <= nx < width -1 and 1 <= ny < height -1:
                if (nx, ny) in walls or (nx, ny) in blizzards_next:
                    continue
                state = (nx, ny, next_t % period)
                if state not in visited:
                    visited.add(state)
                    queue.append((nx, ny, next_t))
            # Optionally, allow waiting at the start or end
    return -1  # If no path found

def main():
    input_file = 'input.txt'
    walls, blizzards, height, width = read_input(input_file)
    start, end = find_start_end(walls, height, width)
    period = compute_period(width -2, height -2)
    blizzard_positions = precompute_blizzards(blizzards, width, height, period)
    
    # Trip 1: Start -> End
    time1 = bfs(start, end, walls, blizzard_positions, period, width, height, 0)
    if time1 == -1:
        print("No path found for trip 1.")
        return
    
    # Trip 2: End -> Start
    time2 = bfs(end, start, walls, blizzard_positions, period, width, height, time1)
    if time2 == -1:
        print("No path found for trip 2.")
        return
    
    # Trip 3: Start -> End
    time3 = bfs(start, end, walls, blizzard_positions, period, width, height, time2)
    if time3 == -1:
        print("No path found for trip 3.")
        return
    
    print(time3)

if __name__ == "__main__":
    main()
