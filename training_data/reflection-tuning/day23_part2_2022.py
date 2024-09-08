def parse_input(filename):
    with open(filename, 'r') as file:
        return {(x, y) for y, line in enumerate(file) for x, char in enumerate(line.strip()) if char == '#'}

def get_adjacent_positions(x, y):
    return [(x-1, y-1), (x, y-1), (x+1, y-1),
            (x-1, y), (x+1, y),
            (x-1, y+1), (x, y+1), (x+1, y+1)]

def propose_move(elf, elves, directions):
    x, y = elf
    if not any((adj_x, adj_y) in elves for adj_x, adj_y in get_adjacent_positions(x, y)):
        return None
    
    for dx, dy, checks in directions:
        if all((x + cx, y + cy) not in elves for cx, cy in checks):
            return (x + dx, y + dy)
    return None

def simulate_round(elves, directions):
    proposed_moves = {}
    for elf in elves:
        move = propose_move(elf, elves, directions)
        if move:
            if move in proposed_moves:
                proposed_moves[move].append(elf)
            else:
                proposed_moves[move] = [elf]
    
    moved = False
    for new_pos, old_positions in proposed_moves.items():
        if len(old_positions) == 1:
            elves.remove(old_positions[0])
            elves.add(new_pos)
            moved = True
    
    directions.append(directions.pop(0))
    return moved

def calculate_empty_tiles(elves):
    min_x = min(x for x, y in elves)
    max_x = max(x for x, y in elves)
    min_y = min(y for x, y in elves)
    max_y = max(y for x, y in elves)
    return (max_x - min_x + 1) * (max_y - min_y + 1) - len(elves)

def solve(elves):
    directions = [
        (0, -1, [(-1, -1), (0, -1), (1, -1)]),  # N
        (0, 1, [(-1, 1), (0, 1), (1, 1)]),      # S
        (-1, 0, [(-1, -1), (-1, 0), (-1, 1)]),  # W
        (1, 0, [(1, -1), (1, 0), (1, 1)])       # E
    ]
    
    rounds = 0
    while True:
        rounds += 1
        moved = simulate_round(elves, directions)
        if rounds == 10:
            part1 = calculate_empty_tiles(elves)
        if not moved:
            return part1, rounds

elves = parse_input("input.txt")
part1, part2 = solve(elves)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
