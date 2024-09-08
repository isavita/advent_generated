import re

def parse_input(filename):
    steps = []
    with open(filename, 'r') as file:
        for line in file:
            state, coords = line.strip().split()
            x, y, z = [list(map(int, re.findall(r'-?\d+', c))) for c in coords.split(',')]
            steps.append((state == 'on', x, y, z))
    return steps

def initialize_reactor(steps):
    grid = [[[False for _ in range(101)] for _ in range(101)] for _ in range(101)]
    
    for state, (x1, x2), (y1, y2), (z1, z2) in steps:
        x1, x2 = max(x1, -50), min(x2, 50)
        y1, y2 = max(y1, -50), min(y2, 50)
        z1, z2 = max(z1, -50), min(z2, 50)
        
        if x1 > 50 or x2 < -50 or y1 > 50 or y2 < -50 or z1 > 50 or z2 < -50:
            continue
        
        for x in range(x1, x2 + 1):
            for y in range(y1, y2 + 1):
                for z in range(z1, z2 + 1):
                    grid[x+50][y+50][z+50] = state

    return sum(sum(sum(row) for row in plane) for plane in grid)

steps = parse_input('input.txt')
result = initialize_reactor(steps)
print(result)
