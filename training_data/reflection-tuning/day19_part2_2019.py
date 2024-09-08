import sys
from collections import deque

def run_intcode(program, inputs):
    pc, rb = 0, 0
    memory = program.copy() + [0] * 10000
    
    def get_param(mode, value):
        if mode == 0: return memory[value]
        if mode == 1: return value
        if mode == 2: return memory[rb + value]
    
    while True:
        opcode = memory[pc] % 100
        modes = [memory[pc] // 100 % 10, memory[pc] // 1000 % 10, memory[pc] // 10000 % 10]
        
        if opcode == 99: return
        elif opcode == 3:
            memory[memory[pc+1] + (rb if modes[0] == 2 else 0)] = inputs.popleft()
            pc += 2
        elif opcode == 4:
            yield get_param(modes[0], memory[pc+1])
            pc += 2
        elif opcode in [1, 2, 7, 8]:
            a, b = get_param(modes[0], memory[pc+1]), get_param(modes[1], memory[pc+2])
            result = a + b if opcode == 1 else a * b if opcode == 2 else int(a < b) if opcode == 7 else int(a == b)
            memory[memory[pc+3] + (rb if modes[2] == 2 else 0)] = result
            pc += 4
        elif opcode in [5, 6]:
            a, b = get_param(modes[0], memory[pc+1]), get_param(modes[1], memory[pc+2])
            pc = b if (a != 0 and opcode == 5) or (a == 0 and opcode == 6) else pc + 3
        elif opcode == 9:
            rb += get_param(modes[0], memory[pc+1])
            pc += 2

def is_affected(x, y, program):
    inputs = deque([x, y])
    return next(run_intcode(program, inputs)) == 1

def solve(program):
    # Part 1
    affected = sum(is_affected(x, y, program) for x in range(50) for y in range(50))
    print(f"Part 1: {affected}")

    # Part 2
    x, y = 0, 99  # Start from a reasonable position
    while True:
        if is_affected(x, y, program):
            if is_affected(x + 99, y - 99, program):
                print(f"Part 2: {x * 10000 + (y - 99)}")
                break
            y += 1
        else:
            x += 1

if __name__ == "__main__":
    with open("input.txt", "r") as f:
        program = list(map(int, f.read().strip().split(',')))
    solve(program)
