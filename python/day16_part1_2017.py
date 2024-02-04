
def spin(programs, X):
    return programs[-X:] + programs[:-X]

def exchange(programs, A, B):
    temp = programs[A]
    programs[A] = programs[B]
    programs[B] = temp
    return programs

def partner(programs, A, B):
    A_index = programs.index(A)
    B_index = programs.index(B)
    return exchange(programs, A_index, B_index)

with open('input.txt', 'r') as file:
    moves = file.read().strip().split(',')

programs = list("abcdefghijklmnop")

for move in moves:
    if move[0] == 's':
        X = int(move[1:])
        programs = spin(programs, X)
    elif move[0] == 'x':
        A, B = map(int, move[1:].split('/'))
        programs = exchange(programs, A, B)
    elif move[0] == 'p':
        A, B = move[1:].split('/')
        programs = partner(programs, A, B)

print(''.join(programs))
