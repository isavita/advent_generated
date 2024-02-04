with open('input.txt') as f:
    lines = f.read().splitlines()

def get_neighbors(x, y, z):
    neighbors = []
    for dz in range(-1, 2):
        for dy in range(-1, 2):
            for dx in range(-1, 2):
                if dx == dy == dz == 0:
                    continue
                neighbors.append((x+dx, y+dy, z+dz))
    return neighbors

active_cubes = set()
for y in range(len(lines)):
    for x in range(len(lines[0])):
        if lines[y][x] == '#':
            active_cubes.add((x, y, 0))

for _ in range(6):
    new_active_cubes = set()
    inactive_neighbors = {}
    
    for cube in active_cubes:
        x, y, z = cube
        count_active_neighbors = 0
        
        for neighbor in get_neighbors(x, y, z):
            if neighbor in active_cubes:
                count_active_neighbors += 1
            else:
                if neighbor in inactive_neighbors:
                    inactive_neighbors[neighbor] += 1
                else:
                    inactive_neighbors[neighbor] = 1
        
        if count_active_neighbors == 2 or count_active_neighbors == 3:
            new_active_cubes.add(cube)
    
    for inactive_cube, count in inactive_neighbors.items():
        if count == 3:
            new_active_cubes.add(inactive_cube)
    
    active_cubes = new_active_cubes

print(len(active_cubes))