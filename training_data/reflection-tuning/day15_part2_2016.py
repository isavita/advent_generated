def parse_input(filename):
    discs = []
    with open(filename, 'r') as file:
        for line in file:
            parts = line.split()
            positions = int(parts[3])
            start_pos = int(parts[-1][:-1])
            discs.append((positions, start_pos))
    return discs

def capsule_passes(discs, start_time):
    for i, (positions, start_pos) in enumerate(discs):
        time = start_time + i + 1
        if (start_pos + time) % positions != 0:
            return False
    return True

def find_solution(discs):
    time = 0
    while True:
        if capsule_passes(discs, time):
            return time
        time += 1

# Part One
discs = parse_input('input.txt')
part1_solution = find_solution(discs)
print(f"Part One: {part1_solution}")

# Part Two
discs.append((11, 0))
part2_solution = find_solution(discs)
print(f"Part Two: {part2_solution}")
