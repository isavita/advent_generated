import re

def manhattan_distance(x1, y1, x2, y2):
    return abs(x1 - x2) + abs(y1 - y2)

def merge_ranges(ranges):
    ranges.sort(key=lambda x: x[0])
    merged = []
    for start, end in ranges:
        if not merged or merged[-1][1] < start - 1:
            merged.append([start, end])
        else:
            merged[-1][1] = max(merged[-1][1], end)
    return merged

def parse_input(filename):
    sensors = []
    with open(filename, 'r') as file:
        for line in file:
            numbers = list(map(int, re.findall(r'-?\d+', line)))
            sensors.append(((numbers[0], numbers[1]), (numbers[2], numbers[3])))
    return sensors

def count_positions(sensors, y):
    ranges = []
    for (sx, sy), (bx, by) in sensors:
        distance = manhattan_distance(sx, sy, bx, by)
        dy = abs(y - sy)
        if dy <= distance:
            dx = distance - dy
            ranges.append((sx - dx, sx + dx))
    merged = merge_ranges(ranges)
    return sum(end - start + 1 for start, end in merged)

def find_distress_beacon(sensors, max_coord):
    for (sx, sy), (bx, by) in sensors:
        distance = manhattan_distance(sx, sy, bx, by)
        for dx in range(distance + 2):
            dy = distance + 1 - dx
            for x, y in [(sx + dx, sy + dy), (sx + dx, sy - dy), (sx - dx, sy + dy), (sx - dx, sy - dy)]:
                if 0 <= x <= max_coord and 0 <= y <= max_coord:
                    if all(manhattan_distance(x, y, sx2, sy2) > manhattan_distance(sx2, sy2, bx2, by2)
                           for (sx2, sy2), (bx2, by2) in sensors):
                        return x * 4000000 + y

sensors = parse_input('input.txt')

# Part 1
print(count_positions(sensors, 2000000))

# Part 2
print(find_distress_beacon(sensors, 4000000))
