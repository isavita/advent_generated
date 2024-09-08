def parse_input(filename):
    with open(filename, 'r') as f:
        grid = f.read().strip().split('\n')
    infected = set()
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell == '#':
                infected.add(complex(x - len(row) // 2, y - len(grid) // 2))
    return infected

def solve_part1(infected, bursts):
    pos = 0
    direction = -1j
    infection_count = 0

    for _ in range(bursts):
        if pos in infected:
            direction *= 1j
            infected.remove(pos)
        else:
            direction *= -1j
            infected.add(pos)
            infection_count += 1
        pos += direction

    return infection_count

def solve_part2(infected, bursts):
    pos = 0
    direction = -1j
    infection_count = 0
    node_states = {node: 2 for node in infected}

    for _ in range(bursts):
        state = node_states.get(pos, 0)
        if state == 0:  # Clean
            direction *= -1j
            node_states[pos] = 1
        elif state == 1:  # Weakened
            node_states[pos] = 2
            infection_count += 1
        elif state == 2:  # Infected
            direction *= 1j
            node_states[pos] = 3
        else:  # Flagged
            direction *= -1
            del node_states[pos]
        pos += direction

    return infection_count

infected = parse_input("input.txt")
print("Part 1:", solve_part1(infected.copy(), 10000))
print("Part 2:", solve_part2(infected, 10000000))
