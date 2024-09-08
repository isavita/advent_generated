import re

def parse_input(filename):
    with open(filename, 'r') as file:
        line = file.read().strip()
    match = re.match(r'target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)', line)
    return tuple(map(int, match.groups()))

def simulate(vx, vy, target):
    x, y = 0, 0
    x_min, x_max, y_min, y_max = target
    max_height = 0
    
    while x <= x_max and y >= y_min:
        x += vx
        y += vy
        max_height = max(max_height, y)
        
        if x_min <= x <= x_max and y_min <= y <= y_max:
            return True, max_height
        
        vx = max(0, vx - 1)
        vy -= 1
    
    return False, max_height

def solve(target):
    x_min, x_max, y_min, y_max = target
    
    # Part 1
    max_height = (abs(y_min) - 1) * abs(y_min) // 2
    
    # Part 2
    count = 0
    for vx in range(1, x_max + 1):
        for vy in range(y_min, abs(y_min)):
            hit, _ = simulate(vx, vy, target)
            if hit:
                count += 1
    
    return max_height, count

target = parse_input("input.txt")
part1, part2 = solve(target)

print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
