
def spin(s, p):
    return p[-s:] + p[:-s]

def exchange(a, b, p):
    p[a], p[b] = p[b], p[a]
    return p

def partner(a, b, p):
    a_idx = p.index(a)
    b_idx = p.index(b)
    p[a_idx], p[b_idx] = p[b_idx], p[a_idx]
    return p

def dance(moves, programs):
    programs = list(programs)
    for move in moves:
        if move[0] == 's':
            programs = spin(int(move[1:]), programs)
        elif move[0] == 'x':
            a, b = map(int, move[1:].split('/'))
            programs = exchange(a, b, programs)
        elif move[0] == 'p':
            a, b = move[1:].split('/')
            programs = partner(a, b, programs)
    return ''.join(programs)

with open('input.txt', 'r') as file:
    moves = file.read().strip().split(',')

programs = 'abcdefghijklmnop'
print(dance(moves, programs))

# Part Two
programs = 'abcdefghijklmnop'
seen = []
for i in range(1000000000):
    if programs in seen:
        programs = seen[1000000000 % i]
        break
    seen.append(programs)
    programs = dance(moves, programs)

print(programs)
