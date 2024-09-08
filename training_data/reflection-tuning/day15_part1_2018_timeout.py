from collections import deque
import copy

def read_input(filename):
    with open(filename, 'r') as f:
        return [list(line.strip()) for line in f]

def find_units(grid):
    units = []
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell in 'EG':
                units.append({'type': cell, 'x': x, 'y': y, 'hp': 200, 'ap': 3})
    return units

def manhattan_distance(a, b):
    return abs(a['x'] - b['x']) + abs(a['y'] - b['y'])

def get_adjacent_positions(x, y):
    return [(x, y-1), (x-1, y), (x+1, y), (x, y+1)]

def find_targets(unit, units):
    return [u for u in units if u['type'] != unit['type'] and u['hp'] > 0]

def find_nearest_target(unit, targets, grid):
    queue = deque([(unit['x'], unit['y'], 0)])
    visited = set()
    while queue:
        x, y, dist = queue.popleft()
        if (x, y) in visited:
            continue
        visited.add((x, y))
        for tx, ty in get_adjacent_positions(x, y):
            if grid[ty][tx] == '.' or (tx, ty) == (unit['x'], unit['y']):
                if any(t['x'] == tx and t['y'] == ty for t in targets):
                    return (tx, ty, dist + 1)
                queue.append((tx, ty, dist + 1))
    return None

def move_unit(unit, target, grid):
    queue = deque([(target[0], target[1], [])])
    visited = set()
    while queue:
        x, y, path = queue.popleft()
        if (x, y) in visited:
            continue
        visited.add((x, y))
        if manhattan_distance({'x': x, 'y': y}, unit) == 1:
            unit['x'], unit['y'] = path[0]
            return
        for nx, ny in get_adjacent_positions(x, y):
            if grid[ny][nx] == '.' or (nx, ny) == (unit['x'], unit['y']):
                new_path = path + [(nx, ny)]
                queue.append((nx, ny, new_path))

def attack(unit, targets, grid):
    adjacent_targets = [t for t in targets if manhattan_distance(unit, t) == 1]
    if adjacent_targets:
        target = min(adjacent_targets, key=lambda t: (t['hp'], t['y'], t['x']))
        target['hp'] -= unit['ap']
        if target['hp'] <= 0:
            grid[target['y']][target['x']] = '.'

def simulate_combat(grid):
    units = find_units(grid)
    rounds = 0
    while True:
        units.sort(key=lambda u: (u['y'], u['x']))
        for unit in units:
            if unit['hp'] <= 0:
                continue
            targets = find_targets(unit, units)
            if not targets:
                return rounds * sum(u['hp'] for u in units if u['hp'] > 0)
            if not any(manhattan_distance(unit, t) == 1 for t in targets):
                nearest = find_nearest_target(unit, targets, grid)
                if nearest:
                    move_unit(unit, nearest, grid)
            attack(unit, targets, grid)
        rounds += 1

def solve(filename):
    grid = read_input(filename)
    return simulate_combat(grid)

print(solve('input.txt'))
