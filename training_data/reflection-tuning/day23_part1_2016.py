def execute_program(instructions):
    registers = {'a': 7, 'b': 0, 'c': 0, 'd': 0}
    pc = 0

    def get_value(x):
        return registers[x] if x in registers else int(x)

    def toggle_instruction(index):
        if 0 <= index < len(instructions):
            parts = instructions[index].split()
            if len(parts) == 2:
                parts[0] = 'dec' if parts[0] == 'inc' else 'inc'
            elif len(parts) == 3:
                parts[0] = 'cpy' if parts[0] == 'jnz' else 'jnz'
            instructions[index] = ' '.join(parts)

    while 0 <= pc < len(instructions):
        parts = instructions[pc].split()
        op = parts[0]

        if op == 'cpy':
            if parts[2] in registers:
                registers[parts[2]] = get_value(parts[1])
        elif op == 'inc':
            if parts[1] in registers:
                registers[parts[1]] += 1
        elif op == 'dec':
            if parts[1] in registers:
                registers[parts[1]] -= 1
        elif op == 'jnz':
            if get_value(parts[1]) != 0:
                pc += get_value(parts[2]) - 1
        elif op == 'tgl':
            toggle_instruction(pc + get_value(parts[1]))

        pc += 1

    return registers['a']

with open('input.txt', 'r') as file:
    instructions = file.read().splitlines()

result = execute_program(instructions)
print(result)
