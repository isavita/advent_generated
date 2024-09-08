def parse_input(data):
    clay = set()
    for line in data.split('\n'):
        a, b = line.split(', ')
        a, a_val = a.split('=')
        b, b_range = b.split('=')
        b_start, b_end = map(int, b_range.split('..'))
        a_val = int(a_val)
        for b_val in range(b_start, b_end + 1):
            if a == 'x':
                clay.add((a_val, b_val))
            else:
                clay.add((b_val, a_val))
    return clay

def simulate_water(clay):
    min_y = min(y for _, y in clay)
    max_y = max(y for _, y in clay)
    
    water = set()
    flowing = set()
    
    def flow(x, y):
        if y > max_y:
            return False
        if (x, y) in clay or (x, y) in water:
            return True
        if y < min_y:
            flowing.add((x, y))
            return False
        
        flowing.add((x, y))
        
        if not flow(x, y + 1):
            return False
        
        left = right = x
        while flow(left - 1, y) and (left - 1, y + 1) not in flowing:
            left -= 1
            flowing.add((left, y))
        while flow(right + 1, y) and (right + 1, y + 1) not in flowing:
            right += 1
            flowing.add((right, y))
        
        if (left - 1, y) in clay and (right + 1, y) in clay:
            for fx in range(left, right + 1):
                water.add((fx, y))
            return True
        return False

    flow(500, 0)
    
    return water, flowing

def solve(data):
    clay = parse_input(data)
    water, flowing = simulate_water(clay)
    
    min_y = min(y for _, y in clay)
    max_y = max(y for _, y in clay)
    
    part1 = sum(1 for x, y in (water | flowing) if min_y <= y <= max_y)
    part2 = sum(1 for x, y in water if min_y <= y <= max_y)
    
    return part1, part2

# Example usage:
data = """x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504"""

part1, part2 = solve(data)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
