
import re

with open('input.txt', 'r') as file:
    data = file.readlines()

reindeer = []
for line in data:
    speed, fly_time, rest_time = map(int, re.findall(r'\d+', line))
    reindeer.append((speed, fly_time, rest_time))

def distance(speed, fly_time, rest_time, total_time):
    cycle_time = fly_time + rest_time
    full_cycles = total_time // cycle_time
    remaining_time = total_time % cycle_time
    distance = full_cycles * fly_time * speed
    distance += min(remaining_time, fly_time) * speed
    return distance

total_time = 2503
distances = [distance(speed, fly_time, rest_time, total_time) for speed, fly_time, rest_time in reindeer]
print(max(distances))

points = [0] * len(reindeer)
for t in range(1, total_time + 1):
    current_distances = [distance(speed, fly_time, rest_time, t) for speed, fly_time, rest_time in reindeer]
    leading_distance = max(current_distances)
    for i in range(len(reindeer)):
        if current_distances[i] == leading_distance:
            points[i] += 1

print(max(points))
