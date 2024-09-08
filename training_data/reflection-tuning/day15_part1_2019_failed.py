from collections import deque

def extend_memory(memory, index):
    if index >= len(memory):
        memory.extend([0] * (index - len(memory) + 1))

def get_param(mode, value, memory, relative_base):
    if mode == 0:
        extend_memory(memory, value)
        return memory[value]
    elif mode == 1:
        return value
    elif mode == 2:
        extend_memory(memory, relative_base + value)
        return memory[relative_base + value]

def set_param(mode, index, value, memory, relative_base):
    if mode == 0:
        extend_memory(memory, index)
        memory[index] = value
    elif mode == 2:
        extend_memory(memory, relative_base + index)
        memory[relative_base + index] = value

def run_intcode(memory, input_value):
    memory = memory.copy()
    pc = 0
    relative_base = 0
    output = None

    while True:
        opcode = memory[pc] % 100
        modes = [int(d) for d in f"{memory[pc]:05d}"[:-2]][::-1]

        if opcode == 99:
            return None, memory, pc, relative_base

        if opcode in [1, 2, 7, 8]:
            a, b = [get_param(modes[i], memory[pc + i + 1], memory, relative_base) for i in range(2)]
            set_param(modes[2], memory[pc + 3], 
                      (a + b) if opcode == 1 else 
                      (a * b) if opcode == 2 else 
                      (1 if (a < b) if opcode == 7 else (a == b) else 0),
                      memory, relative_base)
            pc += 4
        elif opcode in [3, 4]:
            if opcode == 3:
                set_param(modes[0], memory[pc + 1], input_value, memory, relative_base)
            else:
                output = get_param(modes[0], memory[pc + 1], memory, relative_base)
                return output, memory, pc + 2, relative_base
            pc += 2
        elif opcode in [5, 6]:
            a, b = [get_param(modes[i], memory[pc + i + 1], memory, relative_base) for i in range(2)]
            pc = b if (a != 0) if opcode == 5 else (a == 0) else pc + 3
        elif opcode == 9:
            relative_base += get_param(modes[0], memory[pc + 1], memory, relative_base)
            pc += 2

def find_oxygen_system(program):
    directions = {1: (0, 1), 2: (0, -1), 3: (-1, 0), 4: (1, 0)}
    reverse = {1: 2, 2: 1, 3: 4, 4: 3}
    visited = set()
    queue = deque([(0, 0, 0, program, 0, 0)])

    while queue:
        x, y, steps, memory, pc, relative_base = queue.popleft()
        if (x, y) in visited:
            continue
        visited.add((x, y))

        for i in range(1, 5):
            nx, ny = x + directions[i][0], y + directions[i][1]
            if (nx, ny) in visited:
                continue

            status, new_memory, new_pc, new_relative_base = run_intcode(memory, i)

            if status == 0:
                continue
            elif status == 2:
                return steps + 1
            elif status == 1:
                queue.append((nx, ny, steps + 1, new_memory, new_pc, new_relative_base))

    return -1

# Read the input
with open('input.txt', 'r') as file:
    program = list(map(int, file.read().strip().split(',')))

steps = find_oxygen_system(program)
print(f"Fewest number of steps to oxygen system: {steps}")
