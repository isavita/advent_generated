def execute_program(instructions):
    registers = {chr(i): 0 for i in range(ord('a'), ord('i'))}
    mul_count = 0
    ip = 0

    def get_value(x):
        return registers[x] if x.isalpha() else int(x)

    def set_instruction(x, y):
        registers[x] = get_value(y)

    def sub_instruction(x, y):
        registers[x] -= get_value(y)

    def mul_instruction(x, y):
        nonlocal mul_count
        registers[x] *= get_value(y)
        mul_count += 1

    def jnz_instruction(x, y):
        nonlocal ip
        if get_value(x) != 0:
            ip += get_value(y) - 1

    instruction_map = {
        'set': set_instruction,
        'sub': sub_instruction,
        'mul': mul_instruction,
        'jnz': jnz_instruction
    }

    while 0 <= ip < len(instructions):
        inst, *args = instructions[ip].split()
        instruction_map[inst](*args)
        ip += 1

    return mul_count

# Read input from file
with open('input.txt', 'r') as file:
    instructions = file.read().splitlines()

# Execute program and print result
result = execute_program(instructions)
print(result)
