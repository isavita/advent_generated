
import re

instructions = {
    "addr": lambda r, a, b: r[a] + r[b],
    "addi": lambda r, a, b: r[a] + b,
    "mulr": lambda r, a, b: r[a] * r[b],
    "muli": lambda r, a, b: r[a] * b,
    "banr": lambda r, a, b: r[a] & r[b],
    "bani": lambda r, a, b: r[a] & b,
    "borr": lambda r, a, b: r[a] | r[b],
    "bori": lambda r, a, b: r[a] | b,
    "setr": lambda r, a, b: r[a],
    "seti": lambda r, a, b: a,
    "gtir": lambda r, a, b: 1 if a > r[b] else 0,
    "gtri": lambda r, a, b: 1 if r[a] > b else 0,
    "gtrr": lambda r, a, b: 1 if r[a] > r[b] else 0,
    "eqir": lambda r, a, b: 1 if a == r[b] else 0,
    "eqri": lambda r, a, b: 1 if r[a] == b else 0,
    "eqrr": lambda r, a, b: 1 if r[a] == r[b] else 0
}

def load_program(lines):
    program = []
    ip_register = 0
    for line in lines:
        if line.startswith("#ip"):
            ip_register = int(re.findall(r'\d+', line)[0])
            continue
        parts = line.split()
        op = instructions[parts[0]]
        a, b, c = map(int, re.findall(r'\d+', line))
        program.append((op, a, b, c))
    return ip_register, program

def run_program(ip_register, program, registers, max_cycles):
    ip = 0
    cycles = 0
    while ip >= 0 and ip < len(program):
        registers[ip_register] = ip
        op, a, b, c = program[ip]
        registers[c] = op(registers, a, b)
        ip = registers[ip_register] + 1
        cycles += 1
        if max_cycles > 0 and cycles >= max_cycles:
            break
    return registers

def max_value(arr):
    return max(arr)

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        lines = [line.strip() for line in file if line.strip()]
    
    ip_register, program = load_program(lines)

    registers = [0, 0, 0, 0, 0, 0]
    registers[0] = 1
    registers = run_program(ip_register, program, registers, 1000)
    n = max_value(registers)
    total = 0
    for i in range(1, n+1):
        if n % i == 0:
            total += i
    print(total)
