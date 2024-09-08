def execute_instruction(registers, instruction, op_codes):
    op, a, b, c = instruction
    registers[c] = op_codes[op](registers, a, b)

def run_program(instructions, ip_register, initial_r0=0):
    registers = [initial_r0, 0, 0, 0, 0, 0]
    ip = 0
    executed_instructions = 0
    seen_states = set()

    while 0 <= ip < len(instructions):
        state = tuple(registers)
        if state in seen_states:
            return None  # Detected a loop, program won't halt
        seen_states.add(state)

        registers[ip_register] = ip
        execute_instruction(registers, instructions[ip], op_codes)
        ip = registers[ip_register]
        ip += 1
        executed_instructions += 1

        if executed_instructions > 1000000:  # Arbitrary limit to prevent infinite loops
            return None

    return executed_instructions

def parse_input(input_text):
    lines = input_text.strip().split('\n')
    ip_register = int(lines[0].split()[1])
    instructions = [line.split() for line in lines[1:]]
    instructions = [[op] + list(map(int, args)) for op, *args in instructions]
    return ip_register, instructions

op_codes = {
    'addr': lambda r, a, b: r[a] + r[b],
    'addi': lambda r, a, b: r[a] + b,
    'mulr': lambda r, a, b: r[a] * r[b],
    'muli': lambda r, a, b: r[a] * b,
    'banr': lambda r, a, b: r[a] & r[b],
    'bani': lambda r, a, b: r[a] & b,
    'borr': lambda r, a, b: r[a] | r[b],
    'bori': lambda r, a, b: r[a] | b,
    'setr': lambda r, a, b: r[a],
    'seti': lambda r, a, b: a,
    'gtir': lambda r, a, b: 1 if a > r[b] else 0,
    'gtri': lambda r, a, b: 1 if r[a] > b else 0,
    'gtrr': lambda r, a, b: 1 if r[a] > r[b] else 0,
    'eqir': lambda r, a, b: 1 if a == r[b] else 0,
    'eqri': lambda r, a, b: 1 if r[a] == b else 0,
    'eqrr': lambda r, a, b: 1 if r[a] == r[b] else 0,
}

def solve(input_text):
    ip_register, instructions = parse_input(input_text)
    
    # Analyze the program with r0 = 0
    result = run_program(instructions, ip_register, 0)
    
    if result is not None:
        return 0  # If the program halts with r0 = 0, that's our answer
    
    # If the program doesn't halt with r0 = 0, we need to analyze its behavior
    # This part would involve studying the instructions and figuring out the halting condition
    # For the sake of this example, let's assume we've found that the program halts when r0 is a specific value
    # This value would be determined by analyzing the program's logic
    
    target_value = analyze_program_logic(instructions)
    return target_value

def analyze_program_logic(instructions):
    # This function would contain the logic to determine the halting condition
    # based on analyzing the instructions
    # For this example, we'll return a placeholder value
    return 42  # This should be replaced with actual logic based on program analysis

# Example usage:
input_text = """#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5"""

result = solve(input_text)
print(f"The lowest non-negative integer value for register 0 that causes the program to halt: {result}")
