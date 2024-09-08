def parse_instruction(line):
    parts = line.split()
    return parts[0], parts[1:]

def execute(instructions, registers, a_init):
    registers['a'] = a_init
    pc = 0
    
    while pc < len(instructions):
        op, args = instructions[pc]
        
        if op == 'cpy':
            value = registers[args[0]] if args[0] in registers else int(args[0])
            registers[args[1]] = value
        elif op == 'inc':
            registers[args[0]] += 1
        elif op == 'dec':
            registers[args[0]] -= 1
        elif op == 'jnz':
            value = registers[args[0]] if args[0] in registers else int(args[0])
            if value != 0:
                pc += int(args[1]) - 1
        elif op == 'tgl':
            target = pc + registers[args[0]]
            if 0 <= target < len(instructions):
                t_op, t_args = instructions[target]
                if len(t_args) == 1:
                    instructions[target] = ('dec' if t_op == 'inc' else 'inc', t_args)
                else:
                    instructions[target] = ('cpy' if t_op == 'jnz' else 'jnz', t_args)
        
        pc += 1
    
    return registers['a']

def optimize(instructions):
    # Analyze the instructions and optimize if possible
    # This is a placeholder for potential optimizations
    return instructions

def solve(input_data, a_init):
    instructions = [parse_instruction(line.strip()) for line in input_data.split('\n') if line.strip()]
    instructions = optimize(instructions)
    registers = {r: 0 for r in 'abcd'}
    return execute(instructions, registers, a_init)

# Example usage
input_data = """
cpy a b
dec b
cpy a d
cpy 0 a
cpy b c
inc a
dec c
jnz c -2
dec d
jnz d -5
dec b
cpy b c
cpy c d
dec d
inc c
jnz d -2
tgl c
cpy -16 c
jnz 1 c
cpy 91 c
jnz 77 d
inc a
inc d
jnz d -2
inc c
jnz c -5
"""

print("Part 1:", solve(input_data, 7))
print("Part 2:", solve(input_data, 12))
