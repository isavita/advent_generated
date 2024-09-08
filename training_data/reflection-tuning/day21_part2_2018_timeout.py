import re

def parse_input(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
    ip_reg = int(lines[0].split()[1])
    instructions = [line.strip().split() for line in lines[1:]]
    return ip_reg, instructions

def execute_instruction(registers, op, a, b, c):
    if op == 'addr': registers[c] = registers[a] + registers[b]
    elif op == 'addi': registers[c] = registers[a] + b
    elif op == 'mulr': registers[c] = registers[a] * registers[b]
    elif op == 'muli': registers[c] = registers[a] * b
    elif op == 'banr': registers[c] = registers[a] & registers[b]
    elif op == 'bani': registers[c] = registers[a] & b
    elif op == 'borr': registers[c] = registers[a] | registers[b]
    elif op == 'bori': registers[c] = registers[a] | b
    elif op == 'setr': registers[c] = registers[a]
    elif op == 'seti': registers[c] = a
    elif op == 'gtir': registers[c] = 1 if a > registers[b] else 0
    elif op == 'gtri': registers[c] = 1 if registers[a] > b else 0
    elif op == 'gtrr': registers[c] = 1 if registers[a] > registers[b] else 0
    elif op == 'eqir': registers[c] = 1 if a == registers[b] else 0
    elif op == 'eqri': registers[c] = 1 if registers[a] == b else 0
    elif op == 'eqrr': registers[c] = 1 if registers[a] == registers[b] else 0

def run_program(ip_reg, instructions, r0, max_steps=None):
    registers = [r0, 0, 0, 0, 0, 0]
    ip = 0
    steps = 0
    while 0 <= ip < len(instructions):
        if max_steps and steps >= max_steps:
            return None
        registers[ip_reg] = ip
        op, a, b, c = instructions[ip]
        execute_instruction(registers, op, int(a), int(b), int(c))
        ip = registers[ip_reg]
        ip += 1
        steps += 1
    return steps

def solve_part1(ip_reg, instructions):
    r0 = 0
    while True:
        if run_program(ip_reg, instructions, r0) is not None:
            return r0
        r0 += 1

def solve_part2(ip_reg, instructions):
    # Analyze the program to find the pattern
    # This part requires understanding the specific input program
    # For this solution, we'll assume the program has a specific structure
    # that allows us to calculate the maximum r0 value
    
    # Find the instruction that compares r0 with another register
    compare_instruction = None
    for i, (op, a, b, c) in enumerate(instructions):
        if op == 'eqrr' and (a == '0' or b == '0'):
            compare_instruction = i
            break
    
    if compare_instruction is None:
        return "Cannot determine Part 2 solution automatically"

    # Run the program until it reaches the comparison instruction
    registers = [0, 0, 0, 0, 0, 0]
    ip = 0
    while ip != compare_instruction:
        registers[ip_reg] = ip
        op, a, b, c = instructions[ip]
        execute_instruction(registers, op, int(a), int(b), int(c))
        ip = registers[ip_reg]
        ip += 1

    # The value in the register being compared with r0 is likely the maximum r0 value
    compare_reg = 1 if instructions[compare_instruction][1] == '0' else int(instructions[compare_instruction][1])
    return registers[compare_reg]

def main():
    ip_reg, instructions = parse_input('input.txt')
    print(f"Part 1: {solve_part1(ip_reg, instructions)}")
    print(f"Part 2: {solve_part2(ip_reg, instructions)}")

if __name__ == "__main__":
    main()
