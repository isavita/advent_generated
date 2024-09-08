import re

def manhattan_distance(x1, y1, x2, y2):
    return abs(x1 - x2) + abs(y1 - y2)

def parse_input(input_data):
    pattern = r'Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)'
    sensors = []
    for line in input_data.split('\n'):
        match = re.match(pattern, line)
        if match:
            sx, sy, bx, by = map(int, match.groups())
            distance = manhattan_distance(sx, sy, bx, by)
            sensors.append((sx, sy, distance))
    return sensors

def is_covered(x, y, sensors):
    return any(manhattan_distance(sx, sy, x, y) <= d for sx, sy, d in sensors)

def find_distress_beacon(sensors, max_coord):
    for sx, sy, distance in sensors:
        for dx in range(distance + 2):
            dy = (distance + 1) - dx
            for x, y in [(sx + dx, sy + dy), (sx + dx, sy - dy), (sx - dx, sy + dy), (sx - dx, sy - dy)]:
                if 0 <= x <= max_coord and 0 <= y <= max_coord:
                    if not is_covered(x, y, sensors):
                        return x, y
    return None

def solve(input_data):
    sensors = parse_input(input_data)
    max_coord = 4000000
    x, y = find_distress_beacon(sensors, max_coord)
    return x * 4000000 + y

# Example usage:
# result = solve(input_data)
# print(result)
