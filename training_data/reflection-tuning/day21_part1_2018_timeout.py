import re

def parse_input(filename):
    with open(filename, 'r') as f:
        lines = f.readlines()
    ip_reg = int(lines[0].split()[1])
    instructions = [line.strip().split() for line in lines[1:]]
    return ip_reg, instructions

def execute(op, a, b, c, registers):
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

def run_program(ip_reg, instructions, r0):
    registers = [r0, 0, 0, 0, 0, 0]
    ip = 0
    executed_instructions = 0
    while 0 <= ip < len(instructions):
        registers[ip_reg] = ip
        op, a, b, c = instructions[ip]
        execute(op, int(a), int(b), int(c), registers)
        ip = registers[ip_reg]
        ip += 1
        executed_instructions += 1
        if executed_instructions > 1000000:  # Prevent infinite loops
            return None
    return executed_instructions

def solve(filename):
    ip_reg, instructions = parse_input(filename)
    r0 = 0
    while True:
        result = run_program(ip_reg, instructions, r0)
        if result is not None:
            return r0
        r0 += 1

print(solve('input.txt'))
