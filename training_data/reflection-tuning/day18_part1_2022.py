def calculate_surface_area():
    cubes = set()
    
    # Read input from file
    with open('input.txt', 'r') as file:
        for line in file:
            cubes.add(tuple(map(int, line.strip().split(','))))
    
    surface_area = 0
    directions = [
        (1, 0, 0), (-1, 0, 0),
        (0, 1, 0), (0, -1, 0),
        (0, 0, 1), (0, 0, -1)
    ]
    
    for cube in cubes:
        for dx, dy, dz in directions:
            neighbor = (cube[0] + dx, cube[1] + dy, cube[2] + dz)
            if neighbor not in cubes:
                surface_area += 1
    
    return surface_area

print(calculate_surface_area())
