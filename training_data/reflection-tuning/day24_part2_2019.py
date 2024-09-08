def read_input(filename):
    with open(filename, 'r') as f:
        return {(0, r, c) for r, line in enumerate(f.read().strip().split('\n'))
                for c, char in enumerate(line) if char == '#'}

def get_adjacent(level, row, col):
    # Same level
    yield from ((level, r, c) for r, c in [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]
                if 0 <= r < 5 and 0 <= c < 5 and (r, c) != (2, 2))
    
    # Inner level
    if (row, col) == (1, 2): yield from ((level+1, 0, c) for c in range(5))
    if (row, col) == (3, 2): yield from ((level+1, 4, c) for c in range(5))
    if (row, col) == (2, 1): yield from ((level+1, r, 0) for r in range(5))
    if (row, col) == (2, 3): yield from ((level+1, r, 4) for r in range(5))
    
    # Outer level
    if row == 0: yield (level-1, 1, 2)
    if row == 4: yield (level-1, 3, 2)
    if col == 0: yield (level-1, 2, 1)
    if col == 4: yield (level-1, 2, 3)

def evolve(bugs):
    new_bugs = set()
    candidates = set(bugs)
    for bug in bugs:
        candidates.update(get_adjacent(*bug))
    
    for pos in candidates:
        count = sum(adj in bugs for adj in get_adjacent(*pos))
        if pos in bugs and count == 1:
            new_bugs.add(pos)
        elif pos not in bugs and count in (1, 2):
            new_bugs.add(pos)
    
    return new_bugs

def solve(bugs, minutes):
    for _ in range(minutes):
        bugs = evolve(bugs)
    return len(bugs)

bugs = read_input("input.txt")
result = solve(bugs, 200)
print(result)
