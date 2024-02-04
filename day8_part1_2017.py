with open('input.txt', 'r') as file:
    instructions = file.read().strip().split('\n')

registers = {}
max_value = 0

for instruction in instructions:
    parts = instruction.split()
    reg = parts[0]
    op = parts[1]
    val = int(parts[2])
    cond_reg = parts[4]
    cond_op = parts[5]
    cond_val = int(parts[6])

    if reg not in registers:
        registers[reg] = 0
    if cond_reg not in registers:
        registers[cond_reg] = 0

    if eval(f"{registers[cond_reg]} {cond_op} {cond_val}"):
        if op == 'inc':
            registers[reg] += val
        else:
            registers[reg] -= val

        max_value = max(max_value, registers[reg])

print(max(registers.values()))