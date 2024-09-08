def read_instructions(filename):
    with open(filename, 'r') as file:
        return [line.strip().split() for line in file]

def execute_program(instructions, registers):
    pc = 0
    while 0 <= pc < len(instructions):
        inst = instructions[pc]
        if inst[0] == 'hlf':
            registers[inst[1]] //= 2
            pc += 1
        elif inst[0] == 'tpl':
            registers[inst[1]] *= 3
            pc += 1
        elif inst[0] == 'inc':
            registers[inst[1]] += 1
            pc += 1
        elif inst[0] == 'jmp':
            pc += int(inst[1])
        elif inst[0] == 'jie':
            if registers[inst[1][0]] % 2 == 0:
                pc += int(inst[2])
            else:
                pc += 1
        elif inst[0] == 'jio':
            if registers[inst[1][0]] == 1:
                pc += int(inst[2])
            else:
                pc += 1
    return registers['b']

instructions = read_instructions('input.txt')

# Part 1
result_part1 = execute_program(instructions, {'a': 0, 'b': 0})
print(f"Part 1: Value in register b: {result_part1}")

# Part 2
result_part2 = execute_program(instructions, {'a': 1, 'b': 0})
print(f"Part 2: Value in register b: {result_part2}")
