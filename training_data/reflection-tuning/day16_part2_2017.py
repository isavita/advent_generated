def spin(programs, x):
    return programs[-x:] + programs[:-x]

def exchange(programs, a, b):
    programs = list(programs)
    programs[a], programs[b] = programs[b], programs[a]
    return ''.join(programs)

def partner(programs, a, b):
    i, j = programs.index(a), programs.index(b)
    return exchange(programs, i, j)

def dance(programs, moves):
    for move in moves:
        if move[0] == 's':
            programs = spin(programs, int(move[1:]))
        elif move[0] == 'x':
            a, b = map(int, move[1:].split('/'))
            programs = exchange(programs, a, b)
        elif move[0] == 'p':
            a, b = move[1:].split('/')
            programs = partner(programs, a, b)
    return programs

def find_cycle(programs, moves):
    seen = {}
    for i in range(1000000000):  # Arbitrary large number
        if programs in seen:
            return i - seen[programs], i
        seen[programs] = i
        programs = dance(programs, moves)
    return None, None

# Read input
with open('input.txt', 'r') as f:
    moves = f.read().strip().split(',')

# Part One
programs = ''.join(chr(ord('a') + i) for i in range(16))
result_part1 = dance(programs, moves)
print("Part One:", result_part1)

# Part Two
cycle_length, first_occurrence = find_cycle(programs, moves)
remaining_dances = (1000000000 - first_occurrence) % cycle_length

for _ in range(remaining_dances):
    programs = dance(programs, moves)

print("Part Two:", programs)
