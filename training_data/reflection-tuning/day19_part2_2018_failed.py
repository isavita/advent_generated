def execute_instruction(opcode, a, b, c, registers):
    if opcode == 'addr': registers[c] = registers[a] + registers[b]
    elif opcode == 'addi': registers[c] = registers[a] + b
    elif opcode == 'mulr': registers[c] = registers[a] * registers[b]
    elif opcode == 'muli': registers[c] = registers[a] * b
    elif opcode == 'banr': registers[c] = registers[a] & registers[b]
    elif opcode == 'bani': registers[c] = registers[a] & b
    elif opcode == 'borr': registers[c] = registers[a] | registers[b]
    elif opcode == 'bori': registers[c] = registers[a] | b
    elif opcode == 'setr': registers[c] = registers[a]
    elif opcode == 'seti': registers[c] = a
    elif opcode == 'gtir': registers[c] = 1 if a > registers[b] else 0
    elif opcode == 'gtri': registers[c] = 1 if registers[a] > b else 0
    elif opcode == 'gtrr': registers[c] = 1 if registers[a] > registers[b] else 0
    elif opcode == 'eqir': registers[c] = 1 if a == registers[b] else 0
    elif opcode == 'eqri': registers[c] = 1 if registers[a] == b else 0
    elif opcode == 'eqrr': registers[c] = 1 if registers[a] == registers[b] else 0

def run_program(instructions, ip_register, initial_register_0=0):
    registers = [initial_register_0, 0, 0, 0, 0, 0]
    ip = 0
    seen_states = set()
    
    while 0 <= ip < len(instructions):
        state = tuple(registers + [ip])
        if state in seen_states:
            # We've detected a loop, so we can stop
            break
        seen_states.add(state)
        
        registers[ip_register] = ip
        opcode, a, b, c = instructions[ip]
        execute_instruction(opcode, a, b, c, registers)
        ip = registers[ip_register]
        ip += 1
    
    return registers[0]

def parse_input(input_text):
    lines = input_text.strip().split('\n')
    ip_register = int(lines[0].split()[1])
    instructions = [line.split() for line in lines[1:]]
    instructions = [(opcode, int(a), int(b), int(c)) for opcode, a, b, c in instructions]
    return ip_register, instructions

def solve(input_text):
    ip_register, instructions = parse_input(input_text)
    part1 = run_program(instructions, ip_register)
    part2 = run_program(instructions, ip_register, initial_register_0=1)
    return part1, part2

# Example usage:
input_text = """
#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5
"""
part1, part2 = solve(input_text)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
