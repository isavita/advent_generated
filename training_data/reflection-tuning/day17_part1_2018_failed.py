def parse_input(lines):
    clay = set()
    min_y, max_y = float('inf'), 0
    for line in lines:
        parts = line.split(', ')
        if parts[0].startswith('x'):
            x = int(parts[0].split('=')[1])
            y_range = list(map(int, parts[1].split('=')[1].split('..')))
            for y in range(y_range[0], y_range[1] + 1):
                clay.add((x, y))
                min_y = min(min_y, y)
                max_y = max(max_y, y)
        else:
            y = int(parts[0].split('=')[1])
            x_range = list(map(int, parts[1].split('=')[1].split('..')))
            for x in range(x_range[0], x_range[1] + 1):
                clay.add((x, y))
                min_y = min(min_y, y)
                max_y = max(max_y, y)
    return clay, min_y, max_y

def flow_water(clay, min_y, max_y):
    water = set()
    flowing = set()
    
    def pour(x, y):
        if y > max_y:
            return False
        if (x, y) in clay or (x, y) in water:
            return True
        if (x, y) in flowing:
            return False
        
        flowing.add((x, y))
        
        below = pour(x, y + 1)
        if not below:
            return False
        
        left = pour(x - 1, y)
        right = pour(x + 1, y)
        
        if left and right:
            water.add((x, y))
            return True
        return False
    
    pour(500, 0)
    return water, flowing

def count_water_tiles(clay, min_y, max_y):
    water, flowing = flow_water(clay, min_y, max_y)
    return sum(1 for x, y in (water | flowing) if min_y <= y <= max_y)

def solve(input_data):
    clay, min_y, max_y = parse_input(input_data.strip().split('\n'))
    return count_water_tiles(clay, min_y, max_y)

# Example usage:
# input_data = """x=495, y=2..7
# y=7, x=495..501
# x=501, y=3..7
# x=498, y=2..4
# x=506, y=1..2
# x=498, y=10..13
# x=504, y=10..13
# y=13, x=498..504"""
# print(solve(input_data))
