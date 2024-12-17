
def compute_operand(val, a, b, c):
    if val in [0, 1, 2, 3]:
        return val
    if val == 4:
        return a
    if val == 5:
        return b
    if val == 6:
        return c
    raise ValueError(f"Invalid combo operand: {val}")

def simulate_computer(program):
    outs = []
    a, b, c = program['a'], program['b'], program['c']
    input_program = program['program']
    i = 0
    while i < len(input_program):
        cmd = input_program[i]
        i += 1
        if cmd == 0:
            a >>= compute_operand(input_program[i], a, b, c)
        elif cmd == 1:
            b ^= input_program[i]
        elif cmd == 2:
            b = compute_operand(input_program[i], a, b, c) % 8
        elif cmd == 3:
            if a != 0:
                i = input_program[i] - 1
            else:
                i += 1
        elif cmd == 4:
            b ^= c
        elif cmd == 5:
            outs.append(compute_operand(input_program[i], a, b, c) % 8)
        elif cmd == 6:
            b = a >> compute_operand(input_program[i], a, b, c)
        elif cmd == 7:
            c = a >> compute_operand(input_program[i], a, b, c)
        else:
            raise ValueError(f"Invalid opcode: {cmd}")
        if i < len(input_program):
            i += 1
    return outs

def check(program):
    valids = []
    stack = [(0, 0)]
    seen = set()
    
    while stack:
        depth, score = stack.pop()
        if (depth, score) in seen:
            continue
        seen.add((depth, score))
        
        if depth == len(program['program']):
            valids.append(score)
        else:
            for i in range(8):
                new_score = i + 8 * score
                test_program = {'a': new_score, 'b': program['b'], 'c': program['c'], 'program': program['program']}
                result = simulate_computer(test_program)
                if result and result[0] == program['program'][len(program['program']) - 1 - depth]:
                    stack.append((depth + 1, new_score))
    return valids

def main():
    with open("input.txt", "r") as f:
        a, b, c = 0, 0, 0
        program = []
        for line in f:
            line = line.strip()
            if line.startswith("Register A:"):
                a = int(line.split(":")[1].strip())
            elif line.startswith("Register B:"):
                b = int(line.split(":")[1].strip())
            elif line.startswith("Register C:"):
                c = int(line.split(":")[1].strip())
            elif line.startswith("Program:"):
                program = [int(x) for x in line.split(":")[1].strip().split(",")]
    
    p = {'a': a, 'b': b, 'c': c, 'program': program}
    valid_values = check(p)
    print(min(valid_values))

if __name__ == "__main__":
    main()
