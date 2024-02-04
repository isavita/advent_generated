
from collections import defaultdict

def manhattan_distance(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])

points = []
with open('input.txt', 'r') as file:
    for line in file:
        x, y = map(int, line.strip().split(','))
        points.append((x, y))

min_x = min(point[0] for point in points)
max_x = max(point[0] for point in points)
min_y = min(point[1] for point in points)
max_y = max(point[1] for point in points)

area_counts = defaultdict(int)
infinite_areas = set()

for x in range(min_x, max_x+1):
    for y in range(min_y, max_y+1):
        closest_point = None
        min_dist = float('inf')
        equal_dist = False

        for i, point in enumerate(points):
            dist = manhattan_distance((x, y), point)
            if dist < min_dist:
                closest_point = i
                min_dist = dist
                equal_dist = False
            elif dist == min_dist:
                equal_dist = True

        if not equal_dist:
            area_counts[closest_point] += 1

        if x in (min_x, max_x) or y in (min_y, max_y):
            infinite_areas.add(closest_point)

largest_area = max(size for idx, size in area_counts.items() if idx not in infinite_areas)
print(largest_area)
