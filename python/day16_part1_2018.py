
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

operations = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

samples = []
with open('input.txt', 'r') as file:
    lines = file.readlines()
    i = 0
    while i < len(lines):
        if lines[i].startswith("Before:"):
            before = list(map(int, lines[i][9:-2].split(', ')))
            instruction = list(map(int, lines[i+1].split()))
            after = list(map(int, lines[i+2][9:-2].split(', ')))
            samples.append((before, instruction, after))
            i += 4
        else:
            i += 1

count = 0
for sample in samples:
    before, instruction, after = sample
    opcode, A, B, C = instruction[0], instruction[1], instruction[2], instruction[3]
    matches = 0
    for operation in operations:
        registers = before.copy()
        operation(registers, A, B, C)
        if registers == after:
            matches += 1
    if matches >= 3:
        count += 1

print(count)
