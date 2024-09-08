from collections import defaultdict

def get_neighbors(x, y, z):
    for dx in [-1, 0, 1]:
        for dy in [-1, 0, 1]:
            for dz in [-1, 0, 1]:
                if dx != 0 or dy != 0 or dz != 0:
                    yield (x + dx, y + dy, z + dz)

def simulate_cycle(active_cubes):
    neighbor_count = defaultdict(int)
    for cube in active_cubes:
        for neighbor in get_neighbors(*cube):
            neighbor_count[neighbor] += 1
    
    new_active_cubes = set()
    for cube, count in neighbor_count.items():
        if cube in active_cubes and count in [2, 3]:
            new_active_cubes.add(cube)
        elif cube not in active_cubes and count == 3:
            new_active_cubes.add(cube)
    
    return new_active_cubes

def solve():
    with open('input.txt', 'r') as file:
        initial_state = [line.strip() for line in file]
    
    active_cubes = set()
    for y, row in enumerate(initial_state):
        for x, state in enumerate(row):
            if state == '#':
                active_cubes.add((x, y, 0))
    
    for _ in range(6):
        active_cubes = simulate_cycle(active_cubes)
    
    print(len(active_cubes))

solve()
