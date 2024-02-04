
def get_value(registers, x):
    if x.isalpha():
        return registers[x]
    else:
        return int(x)

registers = {'a': 0, 'b': 0, 'c': 0, 'd': 0}
instructions = []

with open('input.txt') as f:
    instructions = f.read().strip().split('\n')

i = 0
while i < len(instructions):
    parts = instructions[i].split()
    if parts[0] == 'cpy':
        registers[parts[2]] = get_value(registers, parts[1])
    elif parts[0] == 'inc':
        registers[parts[1]] += 1
    elif parts[0] == 'dec':
        registers[parts[1]] -= 1
    elif parts[0] == 'jnz':
        if get_value(registers, parts[1]) != 0:
            i += int(parts[2])
            continue
    i += 1

print(registers['a'])
