import re

def read_input(file_path):
    points = []
    with open(file_path, 'r') as file:
        for line in file:
            x, y, vx, vy = map(int, re.findall(r'-?\d+', line))
            points.append([(x, y), (vx, vy)])
    return points

def simulate(points):
    seconds = 0
    while True:
        # Move points
        for point in points:
            position, velocity = point
            point[0] = (position[0] + velocity[0], position[1] + velocity[1])

        # Calculate bounding box
        min_x = min(points, key=lambda p: p[0][0])[0][0]
        max_x = max(points, key=lambda p: p[0][0])[0][0]
        min_y = min(points, key=lambda p: p[0][1])[0][1]
        max_y = max(points, key=lambda p: p[0][1])[0][1]

        # Check for message (when area starts to increase, previous step was the message)
        area = (max_x - min_x) * (max_y - min_y)
        if seconds > 0 and area > prev_area:
            return seconds - 1, prev_points
        prev_area = area
        prev_points = [p[0] for p in points]
        seconds += 1

def print_message(points, second):
    min_x = min(points, key=lambda p: p[0])[0]
    max_x = max(points, key=lambda p: p[0])[0]
    min_y = min(points, key=lambda p: p[1])[1]
    max_y = max(points, key=lambda p: p[1])[1]

    sky = [['.' for _ in range(max_x - min_x + 1)] for _ in range(max_y - min_y + 1)]
    for x, y in points:
        sky[y - min_y][x - min_x] = '#'

    print(f"After {second} seconds:")
    for row in sky:
        print(''.join(row))

if __name__ == "__main__":
    points = read_input("input.txt")
    second, message_points = simulate(points)
    print_message(message_points, second)
