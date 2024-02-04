
from collections import defaultdict

def addr(registers, A, B, C):
    registers[C] = registers[A] + registers[B]

def addi(registers, A, B, C):
    registers[C] = registers[A] + B

def mulr(registers, A, B, C):
    registers[C] = registers[A] * registers[B]

def muli(registers, A, B, C):
    registers[C] = registers[A] * B

def banr(registers, A, B, C):
    registers[C] = registers[A] & registers[B]

def bani(registers, A, B, C):
    registers[C] = registers[A] & B

def borr(registers, A, B, C):
    registers[C] = registers[A] | registers[B]

def bori(registers, A, B, C):
    registers[C] = registers[A] | B

def setr(registers, A, B, C):
    registers[C] = registers[A]

def seti(registers, A, B, C):
    registers[C] = A

def gtir(registers, A, B, C):
    registers[C] = 1 if A > registers[B] else 0

def gtri(registers, A, B, C):
    registers[C] = 1 if registers[A] > B else 0

def gtrr(registers, A, B, C):
    registers[C] = 1 if registers[A] > registers[B] else 0

def eqir(registers, A, B, C):
    registers[C] = 1 if A == registers[B] else 0

def eqri(registers, A, B, C):
    registers[C] = 1 if registers[A] == B else 0

def eqrr(registers, A, B, C):
    registers[C] = 1 if registers[A] == registers[B] else 0

opcodes = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

def test_opcode(sample, opcode):
    registers = sample[0][:]
    opcode(registers, sample[1][1], sample[1][2], sample[1][3])
    return registers == sample[2]

def parse_input(input_file):
    samples = []
    with open(input_file, 'r') as file:
        while True:
            before_line = file.readline().strip()
            if not before_line:
                break
            before = list(map(int, before_line[9:-1].split(', ')))
            instruction = list(map(int, file.readline().strip().split()))
            after = list(map(int, file.readline().strip()[9:-1].split(', ')))
            samples.append((before, instruction, after))
            file.readline()  # read blank line
    return samples

def part_one(samples):
    count = 0
    for sample in samples:
        num_behave_like = 0
        for opcode in opcodes:
            if test_opcode(sample, opcode):
                num_behave_like += 1
        if num_behave_like >= 3:
            count += 1
    return count

def part_two(samples, program):
    opcode_mapping = {}
    possible_opcodes = defaultdict(set)
    
    for sample in samples:
        opcode_num = sample[1][0]
        possible_opcodes[opcode_num] |= {opcode for opcode in opcodes if test_opcode(sample, opcode)}
    
    while len(opcode_mapping) < 16:
        for opcode_num, opcodes_set in possible_opcodes.items():
            if len(opcodes_set) == 1:
                opcode = opcodes_set.pop()
                opcode_mapping[opcode_num] = opcode
                for other_opcodes in possible_opcodes.values():
                    other_opcodes.discard(opcode)
    
    registers = [0, 0, 0, 0]
    for instruction in program:
        opcode = opcode_mapping[instruction[0]]
        opcode(registers, instruction[1], instruction[2], instruction[3])
    
    return registers[0]

if __name__ == "__main__":
    samples = parse_input("input.txt")
    program = []
    with open("input.txt", 'r') as file:
        for line in file:
            if line[0].isdigit():
                program.append(list(map(int, line.strip().split())))
    
    print(part_one(samples))
    print(part_two(samples, program))
