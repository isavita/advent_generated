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

def count_positions(ranges):
    return sum(end - start + 1 for start, end in ranges)

target_y = 2000000
sensors = []
beacons_on_target = set()

with open('input.txt', 'r') as file:
    for line in file:
        numbers = list(map(int, re.findall(r'-?\d+', line)))
        sx, sy, bx, by = numbers
        distance = manhattan_distance(sx, sy, bx, by)
        sensors.append((sx, sy, distance))
        if by == target_y:
            beacons_on_target.add(bx)

ranges = []
for sx, sy, distance in sensors:
    dy = abs(sy - target_y)
    if dy <= distance:
        dx = distance - dy
        ranges.append((sx - dx, sx + dx))

merged_ranges = merge_ranges(ranges)
total_positions = count_positions(merged_ranges) - len(beacons_on_target)

print(total_positions)
