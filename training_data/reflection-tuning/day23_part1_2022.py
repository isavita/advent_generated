from collections import defaultdict, deque

def parse_input(filename):
    elves = set()
    with open(filename, 'r') as f:
        for y, line in enumerate(f):
            for x, char in enumerate(line.strip()):
                if char == '#':
                    elves.add((x, y))
    return elves

def get_adjacent(x, y):
    return [(x-1, y-1), (x, y-1), (x+1, y-1),
            (x-1, y), (x+1, y),
            (x-1, y+1), (x, y+1), (x+1, y+1)]

def propose_move(elf, elves, directions):
    x, y = elf
    if not any((adj in elves) for adj in get_adjacent(x, y)):
        return None
    
    for dx, dy, checks in directions:
        if not any((x+cx, y+cy) in elves for cx, cy in checks):
            return (x+dx, y+dy)
    return None

def simulate_round(elves, directions):
    proposals = defaultdict(list)
    for elf in elves:
        prop = propose_move(elf, elves, directions)
        if prop:
            proposals[prop].append(elf)
    
    for new_pos, old_positions in proposals.items():
        if len(old_positions) == 1:
            elves.remove(old_positions[0])
            elves.add(new_pos)
    
    directions.rotate(-1)

def get_bounds(elves):
    xs, ys = zip(*elves)
    return min(xs), max(xs), min(ys), max(ys)

def count_empty_tiles(elves):
    min_x, max_x, min_y, max_y = get_bounds(elves)
    return (max_x - min_x + 1) * (max_y - min_y + 1) - len(elves)

def solve(filename):
    elves = parse_input(filename)
    directions = deque([
        (0, -1, [(-1, -1), (0, -1), (1, -1)]),  # N
        (0, 1, [(-1, 1), (0, 1), (1, 1)]),      # S
        (-1, 0, [(-1, -1), (-1, 0), (-1, 1)]),  # W
        (1, 0, [(1, -1), (1, 0), (1, 1)])       # E
    ])
    
    for _ in range(10):
        simulate_round(elves, directions)
    
    return count_empty_tiles(elves)

print(solve('input.txt'))
