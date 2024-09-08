from itertools import product

def parse_input(filename):
    active = set()
    with open(filename, 'r') as f:
        for y, line in enumerate(f):
            for x, char in enumerate(line.strip()):
                if char == '#':
                    active.add((x, y, 0, 0))
    return active

def get_neighbors(cube):
    return product(*(range(c-1, c+2) for c in cube))

def count_active_neighbors(cube, active):
    return sum(1 for n in get_neighbors(cube) if n != cube and n in active)

def simulate_cycle(active, is_4d=False):
    new_active = set()
    to_check = set(n for cube in active for n in get_neighbors(cube))
    
    for cube in to_check:
        count = count_active_neighbors(cube, active)
        if cube in active:
            if count in (2, 3):
                new_active.add(cube)
        elif count == 3:
            new_active.add(cube)
    
    if not is_4d:
        new_active = {(x, y, z, 0) for x, y, z, _ in new_active}
    
    return new_active

def run_simulation(active, cycles=6, is_4d=False):
    for _ in range(cycles):
        active = simulate_cycle(active, is_4d)
    return len(active)

active_cubes = parse_input('input.txt')

# Part 1
print(run_simulation(active_cubes))

# Part 2
print(run_simulation(active_cubes, is_4d=True))
