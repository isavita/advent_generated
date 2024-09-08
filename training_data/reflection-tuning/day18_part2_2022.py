from collections import deque

def read_input(filename):
    with open(filename, 'r') as file:
        return {tuple(map(int, line.strip().split(','))) for line in file}

def get_neighbors(x, y, z):
    return [(x+1,y,z), (x-1,y,z), (x,y+1,z), (x,y-1,z), (x,y,z+1), (x,y,z-1)]

def solve(cubes):
    surface_area = 0
    for cube in cubes:
        surface_area += sum(1 for neighbor in get_neighbors(*cube) if neighbor not in cubes)
    
    # Find bounds
    mins = [min(coord[i] for coord in cubes) - 1 for i in range(3)]
    maxs = [max(coord[i] for coord in cubes) + 1 for i in range(3)]
    
    # Flood fill
    queue = deque([tuple(mins)])
    visited = set()
    exterior_surface_area = 0
    
    while queue:
        current = queue.popleft()
        if current in visited:
            continue
        visited.add(current)
        
        for neighbor in get_neighbors(*current):
            if all(mins[i] <= neighbor[i] <= maxs[i] for i in range(3)):
                if neighbor in cubes:
                    exterior_surface_area += 1
                elif neighbor not in visited:
                    queue.append(neighbor)
    
    return surface_area, exterior_surface_area

cubes = read_input("input.txt")
part1, part2 = solve(cubes)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
