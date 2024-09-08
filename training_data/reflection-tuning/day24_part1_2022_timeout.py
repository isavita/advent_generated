from collections import deque
import math

def parse_input(filename):
    with open(filename, 'r') as file:
        grid = [list(line.strip()) for line in file]
    
    start = (0, grid[0].index('.'))
    end = (len(grid) - 1, grid[-1].index('.'))
    
    blizzards = []
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            if grid[i][j] in '^v<>':
                blizzards.append((i, j, grid[i][j]))
    
    return grid, start, end, blizzards

def lcm(a, b):
    return abs(a * b) // math.gcd(a, b)

def move_blizzard(pos, direction, time, height, width):
    i, j = pos
    if direction == '^':
        return ((i - time - 1) % (height - 2) + 1, j)
    elif direction == 'v':
        return ((i + time - 1) % (height - 2) + 1, j)
    elif direction == '<':
        return (i, (j - time - 1) % (width - 2) + 1)
    elif direction == '>':
        return (i, (j + time - 1) % (width - 2) + 1)

def get_blizzard_positions(blizzards, time, height, width):
    return {move_blizzard((i, j), d, time, height, width) for i, j, d in blizzards}

def bfs(grid, start, end, blizzards):
    height, width = len(grid), len(grid[0])
    cycle = lcm(height - 2, width - 2)
    queue = deque([(start, 0)])
    visited = set()
    
    while queue:
        pos, time = queue.popleft()
        
        if pos == end:
            return time
        
        if (pos, time % cycle) in visited:
            continue
        
        visited.add((pos, time % cycle))
        
        blizzard_pos = get_blizzard_positions(blizzards, time + 1, height, width)
        
        for di, dj in [(0, 0), (0, 1), (0, -1), (1, 0), (-1, 0)]:
            new_pos = (pos[0] + di, pos[1] + dj)
            
            if (0 <= new_pos[0] < height and 0 <= new_pos[1] < width and
                grid[new_pos[0]][new_pos[1]] != '#' and
                new_pos not in blizzard_pos):
                queue.append((new_pos, time + 1))
    
    return -1  # No path found

def main():
    grid, start, end, blizzards = parse_input("input.txt")
    result = bfs(grid, start, end, blizzards)
    print(result)

if __name__ == "__main__":
    main()
