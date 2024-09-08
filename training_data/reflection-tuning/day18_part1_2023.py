def parse_line(line):
    direction, distance, color = line.split()
    return direction, int(distance), color.strip('(#)')

def calculate_area(instructions):
    x, y = 0, 0
    points = [(0, 0)]
    perimeter = 0

    for direction, distance, _ in instructions:
        if direction == 'R':
            x += distance
        elif direction == 'L':
            x -= distance
        elif direction == 'U':
            y -= distance
        elif direction == 'D':
            y += distance
        
        points.append((x, y))
        perimeter += distance

    # Shoelace formula
    area = 0
    for i in range(len(points)):
        j = (i + 1) % len(points)
        area += points[i][0] * points[j][1]
        area -= points[j][0] * points[i][1]
    area = abs(area) // 2

    # Pick's theorem: A = i + b/2 - 1
    # where A is the area, i is the number of interior points, and b is the number of boundary points
    # Rearranging: i = A - b/2 + 1
    interior_points = area - perimeter // 2 + 1

    # Total points = interior points + boundary points
    total_points = interior_points + perimeter

    return total_points

# Read input from file
with open('input.txt', 'r') as file:
    instructions = [parse_line(line.strip()) for line in file]

# Calculate and print the result
result = calculate_area(instructions)
print(result)
