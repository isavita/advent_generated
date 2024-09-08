def parse_instruction(hex_code):
    hex_code = hex_code[1:]  # Remove the '#'
    distance = int(hex_code[:5], 16)
    direction = int(hex_code[5])
    return ['RDLU'[direction], distance]

def shoelace_area(vertices):
    n = len(vertices)
    area = 0
    for i in range(n):
        j = (i + 1) % n
        area += vertices[i][0] * vertices[j][1]
        area -= vertices[j][0] * vertices[i][1]
    return abs(area) // 2

def calculate_lagoon_volume(instructions):
    x, y = 0, 0
    vertices = [(0, 0)]
    perimeter = 0

    for direction, distance in instructions:
        if direction == 'R':
            x += distance
        elif direction == 'L':
            x -= distance
        elif direction == 'U':
            y -= distance
        elif direction == 'D':
            y += distance
        
        vertices.append((x, y))
        perimeter += distance

    area = shoelace_area(vertices)
    # Use Pick's theorem to calculate the number of interior points
    interior_points = area - perimeter // 2 + 1
    
    # Total volume is interior points plus the perimeter
    return interior_points + perimeter

# Read and parse instructions
with open('input.txt', 'r') as file:
    instructions = [parse_instruction(line.split()[-1]) for line in file]

# Calculate and print the result
result = calculate_lagoon_volume(instructions)
print(f"The lagoon could hold {result} cubic meters of lava.")
