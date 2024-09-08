def spin(programs, x):
    return programs[-x:] + programs[:-x]

def exchange(programs, a, b):
    programs[a], programs[b] = programs[b], programs[a]
    return programs

def partner(programs, a, b):
    i, j = programs.index(a), programs.index(b)
    programs[i], programs[j] = programs[j], programs[i]
    return programs

# Read input
with open('input.txt', 'r') as file:
    moves = file.read().strip().split(',')

# Initialize programs
programs = list('abcdefghijklmnop')

# Apply dance moves
for move in moves:
    if move[0] == 's':
        programs = spin(programs, int(move[1:]))
    elif move[0] == 'x':
        a, b = map(int, move[1:].split('/'))
        programs = exchange(programs, a, b)
    elif move[0] == 'p':
        a, b = move[1:].split('/')
        programs = partner(programs, a, b)

# Print final order
print(''.join(programs))
