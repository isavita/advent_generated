with open('input.txt') as f:
    coordinates = [tuple(map(int, line.split(', '))) for line in f]

max_x = max(coord[0] for coord in coordinates)
max_y = max(coord[1] for coord in coordinates)

def manhattan_distance(coord1, coord2):
    return abs(coord1[0] - coord2[0]) + abs(coord1[1] - coord2[1])

def closest_coordinate(point):
    min_dist = float('inf')
    closest_coord = None
    for i, coord in enumerate(coordinates):
        dist = manhattan_distance(point, coord)
        if dist < min_dist:
            min_dist = dist
            closest_coord = i
        elif dist == min_dist:
            closest_coord = None
    return closest_coord

infinite_areas = set()
areas = [0] * len(coordinates)
region_size = 0

for x in range(max_x + 1):
    for y in range(max_y + 1):
        total_dist = sum(manhattan_distance((x, y), coord) for coord in coordinates)
        if total_dist < 10000:
            region_size += 1
        closest = closest_coordinate((x, y))
        if closest is not None:
            areas[closest] += 1
            if x == 0 or x == max_x or y == 0 or y == max_y:
                infinite_areas.add(closest)

largest_area = max(area for i, area in enumerate(areas) if i not in infinite_areas)

print(largest_area)
print(region_size)