def parse_input(filename):
    with open(filename, 'r') as f:
        lines = f.readlines()
    ip_bind = int(lines[0].split()[1])
    instructions = [line.strip().split() for line in lines[1:]]
    return ip_bind, instructions

def execute_instruction(opcode, a, b, c, registers):
    if opcode == 'addr':
        registers[c] = registers[a] + registers[b]
    elif opcode == 'addi':
        registers[c] = registers[a] + b
    elif opcode == 'mulr':
        registers[c] = registers[a] * registers[b]
    elif opcode == 'muli':
        registers[c] = registers[a] * b
    elif opcode == 'banr':
        registers[c] = registers[a] & registers[b]
    elif opcode == 'bani':
        registers[c] = registers[a] & b
    elif opcode == 'borr':
        registers[c] = registers[a] | registers[b]
    elif opcode == 'bori':
        registers[c] = registers[a] | b
    elif opcode == 'setr':
        registers[c] = registers[a]
    elif opcode == 'seti':
        registers[c] = a
    elif opcode == 'gtir':
        registers[c] = 1 if a > registers[b] else 0
    elif opcode == 'gtri':
        registers[c] = 1 if registers[a] > b else 0
    elif opcode == 'gtrr':
        registers[c] = 1 if registers[a] > registers[b] else 0
    elif opcode == 'eqir':
        registers[c] = 1 if a == registers[b] else 0
    elif opcode == 'eqri':
        registers[c] = 1 if registers[a] == b else 0
    elif opcode == 'eqrr':
        registers[c] = 1 if registers[a] == registers[b] else 0

def run_program(ip_bind, instructions, initial_reg0=0):
    registers = [initial_reg0, 0, 0, 0, 0, 0]
    ip = 0
    while 0 <= ip < len(instructions):
        registers[ip_bind] = ip
        opcode, *params = instructions[ip]
        execute_instruction(opcode, int(params[0]), int(params[1]), int(params[2]), registers)
        ip = registers[ip_bind]
        ip += 1
    return registers[0]

ip_bind, instructions = parse_input('input.txt')

# Part 1
result_part1 = run_program(ip_bind, instructions)
print(f"Part 1: {result_part1}")

# Part 2
result_part2 = run_program(ip_bind, instructions, initial_reg0=1)
print(f"Part 2: {result_part2}")
