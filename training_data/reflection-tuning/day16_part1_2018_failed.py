def addr(reg, a, b, c): reg[c] = reg[a] + reg[b]
def addi(reg, a, b, c): reg[c] = reg[a] + b
def mulr(reg, a, b, c): reg[c] = reg[a] * reg[b]
def muli(reg, a, b, c): reg[c] = reg[a] * b
def banr(reg, a, b, c): reg[c] = reg[a] & reg[b]
def bani(reg, a, b, c): reg[c] = reg[a] & b
def borr(reg, a, b, c): reg[c] = reg[a] | reg[b]
def bori(reg, a, b, c): reg[c] = reg[a] | b
def setr(reg, a, b, c): reg[c] = reg[a]
def seti(reg, a, b, c): reg[c] = a
def gtir(reg, a, b, c): reg[c] = 1 if a > reg[b] else 0
def gtri(reg, a, b, c): reg[c] = 1 if reg[a] > b else 0
def gtrr(reg, a, b, c): reg[c] = 1 if reg[a] > reg[b] else 0
def eqir(reg, a, b, c): reg[c] = 1 if a == reg[b] else 0
def eqri(reg, a, b, c): reg[c] = 1 if reg[a] == b else 0
def eqrr(reg, a, b, c): reg[c] = 1 if reg[a] == reg[b] else 0

opcodes = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

def parse_input(input_text):
    samples = []
    lines = input_text.strip().split('\n')
    i = 0
    while i < len(lines) and lines[i].startswith('Before'):
        before = eval(lines[i][8:])
        instruction = list(map(int, lines[i+1].split()))
        after = eval(lines[i+2][8:])
        samples.append((before, instruction, after))
        i += 4
    return samples

def count_matching_opcodes(sample):
    before, (_, a, b, c), after = sample
    count = 0
    for opcode in opcodes:
        reg = before.copy()
        opcode(reg, a, b, c)
        if reg == after:
            count += 1
    return count

def solve(input_text):
    samples = parse_input(input_text)
    return sum(1 for sample in samples if count_matching_opcodes(sample) >= 3)

# Example usage:
input_text = """Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]

Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]"""

result = solve(input_text)
print(result)
