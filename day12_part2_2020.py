
from math import sin, cos, radians

directions = [(1, 0), (0, -1), (-1, 0), (0, 1)]
x, y = 0, 0
waypoint_x, waypoint_y = 10, 1
facing = 0

with open('input.txt') as f:
    for line in f:
        action, value = line[0], int(line[1:])
        if action == 'N':
            waypoint_y += value
        elif action == 'S':
            waypoint_y -= value
        elif action == 'E':
            waypoint_x += value
        elif action == 'W':
            waypoint_x -= value
        elif action == 'L':
            for _ in range(value // 90):
                waypoint_x, waypoint_y = -waypoint_y, waypoint_x
        elif action == 'R':
            for _ in range(value // 90):
                waypoint_x, waypoint_y = waypoint_y, -waypoint_x
        elif action == 'F':
            x += waypoint_x * value
            y += waypoint_y * value

print(abs(x) + abs(y))
