
def solve():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    A, B, C = 0, 0, 0
    program = []

    for line in lines:
        line = line.strip()
        if not line:
            continue
        if line.startswith("Register A:"):
            A = int(line.split(":")[1].strip())
        elif line.startswith("Register B:"):
            B = int(line.split(":")[1].strip())
        elif line.startswith("Register C:"):
            C = int(line.split(":")[1].strip())
        elif line.startswith("Program:"):
            program = [int(x.strip()) for x in line.split(":")[1].strip().split(",")]

    def get_combo_val(op):
        if op <= 3:
            return op
        if op == 4:
            return A
        if op == 5:
            return B
        if op == 6:
            return C
        raise ValueError("invalid combo operand")

    output_vals = []
    ip = 0
    while ip < len(program):
        opcode = program[ip]
        if ip + 1 >= len(program):
            break
        operand = program[ip + 1]

        if opcode == 0:
            den = get_combo_val(operand)
            A = A // (1 << den) if den != 0 else 0
            ip += 2
        elif opcode == 1:
            B ^= operand
            ip += 2
        elif opcode == 2:
            B = get_combo_val(operand) % 8
            ip += 2
        elif opcode == 3:
            ip = operand if A != 0 else ip + 2
        elif opcode == 4:
            B ^= C
            ip += 2
        elif opcode == 5:
            output_vals.append(str(get_combo_val(operand) % 8))
            ip += 2
        elif opcode == 6:
            B = A // (1 << get_combo_val(operand))
            ip += 2
        elif opcode == 7:
            C = A // (1 << get_combo_val(operand))
            ip += 2
        else:
            break

    print(",".join(output_vals))

solve()
