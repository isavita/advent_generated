import re

def parse_input(filename):
    with open(filename, 'r') as f:
        content = f.read().split('\n\n\n\n')
    samples = [list(map(eval, re.findall(r'\[.*?\]', sample))) + [list(map(int, re.findall(r'\d+', sample.split('\n')[1])))] for sample in content[0].split('\n\n')]
    program = [list(map(int, line.split())) for line in content[1].strip().split('\n')]
    return samples, program

def addr(r, a, b, c): r[c] = r[a] + r[b]
def addi(r, a, b, c): r[c] = r[a] + b
def mulr(r, a, b, c): r[c] = r[a] * r[b]
def muli(r, a, b, c): r[c] = r[a] * b
def banr(r, a, b, c): r[c] = r[a] & r[b]
def bani(r, a, b, c): r[c] = r[a] & b
def borr(r, a, b, c): r[c] = r[a] | r[b]
def bori(r, a, b, c): r[c] = r[a] | b
def setr(r, a, b, c): r[c] = r[a]
def seti(r, a, b, c): r[c] = a
def gtir(r, a, b, c): r[c] = 1 if a > r[b] else 0
def gtri(r, a, b, c): r[c] = 1 if r[a] > b else 0
def gtrr(r, a, b, c): r[c] = 1 if r[a] > r[b] else 0
def eqir(r, a, b, c): r[c] = 1 if a == r[b] else 0
def eqri(r, a, b, c): r[c] = 1 if r[a] == b else 0
def eqrr(r, a, b, c): r[c] = 1 if r[a] == r[b] else 0

opcodes = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

def count_matching_opcodes(before, instruction, after):
    count = 0
    for op in opcodes:
        registers = before.copy()
        op(registers, *instruction[1:])
        if registers == after:
            count += 1
    return count

def deduce_opcodes(samples):
    possible_opcodes = {i: set(range(16)) for i in range(16)}
    for before, after, instruction in samples:
        opcode = instruction[0]
        for i, op in enumerate(opcodes):
            registers = before.copy()
            op(registers, *instruction[1:])
            if registers != after:
                possible_opcodes[opcode].discard(i)
    
    final_opcodes = {}
    while len(final_opcodes) < 16:
        for opcode, possibilities in possible_opcodes.items():
            if len(possibilities) == 1:
                final_opcodes[opcode] = possibilities.pop()
                for other_possibilities in possible_opcodes.values():
                    other_possibilities.discard(final_opcodes[opcode])
    
    return final_opcodes

def execute_program(program, opcode_map):
    registers = [0, 0, 0, 0]
    for instruction in program:
        opcodes[opcode_map[instruction[0]]](registers, *instruction[1:])
    return registers[0]

samples, program = parse_input('input.txt')

# Part 1
part1_answer = sum(1 for sample in samples if count_matching_opcodes(*sample) >= 3)
print(f"Part 1 Answer: {part1_answer}")

# Part 2
opcode_map = deduce_opcodes(samples)
part2_answer = execute_program(program, opcode_map)
print(f"Part 2 Answer: {part2_answer}")
