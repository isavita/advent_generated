from collections import defaultdict

def run_intcode(memory, inputs):
    pc = 0
    relative_base = 0
    outputs = []

    def get_param(mode, value):
        if mode == 0:
            return memory[value]
        elif mode == 1:
            return value
        elif mode == 2:
            return memory[relative_base + value]

    def set_param(mode, value, data):
        if mode == 0:
            memory[value] = data
        elif mode == 2:
            memory[relative_base + value] = data

    while True:
        instruction = memory[pc]
        opcode = instruction % 100
        modes = [int(d) for d in str(instruction // 100).zfill(3)][::-1]

        if opcode == 99:
            break

        if opcode in [1, 2, 7, 8]:
            a = get_param(modes[0], memory[pc + 1])
            b = get_param(modes[1], memory[pc + 2])
            if opcode == 1:
                result = a + b
            elif opcode == 2:
                result = a * b
            elif opcode == 7:
                result = int(a < b)
            else:
                result = int(a == b)
            set_param(modes[2], memory[pc + 3], result)
            pc += 4
        elif opcode in [3, 4]:
            if opcode == 3:
                set_param(modes[0], memory[pc + 1], inputs.pop(0))
            else:
                outputs.append(get_param(modes[0], memory[pc + 1]))
            pc += 2
        elif opcode in [5, 6]:
            a = get_param(modes[0], memory[pc + 1])
            b = get_param(modes[1], memory[pc + 2])
            if (opcode == 5 and a != 0) or (opcode == 6 and a == 0):
                pc = b
            else:
                pc += 3
        elif opcode == 9:
            relative_base += get_param(modes[0], memory[pc + 1])
            pc += 2

    return outputs

def solve(memory, part2=False):
    if not part2:
        script = [
            "NOT A J",
            "NOT B T",
            "OR T J",
            "NOT C T",
            "OR T J",
            "AND D J",
            "WALK"
        ]
    else:
        script = [
            "NOT A J",
            "NOT B T",
            "OR T J",
            "NOT C T",
            "OR T J",
            "AND D J",
            "NOT E T",
            "NOT T T",
            "OR H T",
            "AND T J",
            "RUN"
        ]

    inputs = [ord(c) for line in script for c in line + '\n']
    outputs = run_intcode(memory, inputs)

    if outputs[-1] > 127:
        return outputs[-1]
    else:
        print(''.join(chr(c) for c in outputs))
        return None

# Read input
with open("input.txt", "r") as f:
    memory = defaultdict(int, {i: int(x) for i, x in enumerate(f.read().strip().split(','))})

# Solve Part 1
print("Part 1:", solve(memory.copy()))

# Solve Part 2
print("Part 2:", solve(memory.copy(), part2=True))
