
with open("input.txt", "r") as file:
    lines = [line.strip() for line in file]

def get_points_with_steps(path):
    points = {}
    current = [0, 0]
    steps = 0
    for move in path.split(","):
        direction = move[0]
        distance = int(move[1:])
        for _ in range(distance):
            steps += 1
            if direction == 'U':
                current[1] += 1
            elif direction == 'D':
                current[1] -= 1
            elif direction == 'L':
                current[0] -= 1
            elif direction == 'R':
                current[0] += 1
            if tuple(current) not in points:
                points[tuple(current)] = steps
    return points

wire1 = get_points_with_steps(lines[0])
wire2 = get_points_with_steps(lines[1])

min_steps = float('inf')
for point, steps1 in wire1.items():
    if point in wire2:
        steps2 = wire2[point]
        total_steps = steps1 + steps2
        if total_steps < min_steps:
            min_steps = total_steps

print(min_steps)
