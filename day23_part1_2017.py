with open('input.txt', 'r') as f:
    instructions = f.read().strip().split('\n')

registers = {char: 0 for char in 'abcdefgh'}

def get_value(val):
    try:
        return int(val)
    except ValueError:
        return registers[val]

def run_program():
    mul_invoked = 0
    i = 0
    while i < len(instructions):
        instr, X, Y = instructions[i].split()
        if instr == 'set':
            registers[X] = get_value(Y)
        elif instr == 'sub':
            registers[X] -= get_value(Y)
        elif instr == 'mul':
            registers[X] *= get_value(Y)
            mul_invoked += 1
        elif instr == 'jnz':
            if get_value(X) != 0:
                i += get_value(Y)
                continue
        i += 1
    return mul_invoked

print(run_program())