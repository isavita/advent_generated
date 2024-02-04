
import re

with open('input.txt') as f:
    data = f.readlines()

points = []
velocities = []

for line in data:
    x, y, vx, vy = map(int, re.findall(r'-?\d+', line))
    points.append((x, y))
    velocities.append((vx, vy))

time = 0

while True:
    min_x = min(points, key=lambda x: x[0])[0]
    max_x = max(points, key=lambda x: x[0])[0]
    min_y = min(points, key=lambda x: x[1])[1]
    max_y = max(points, key=lambda x: x[1])[1]

    if max_x - min_x < 100 and max_y - min_y < 10:
        break

    for i in range(len(points)):
        points[i] = (points[i][0] + velocities[i][0], points[i][1] + velocities[i][1])

    time += 1

print(time)
