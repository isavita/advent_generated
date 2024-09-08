def analyze_program(instructions):
    # Analyze the program to find the key register and termination condition
    # This is a simplified example; actual analysis would be more complex
    key_register = 1  # Assume register 1 controls the main loop
    termination_value = max(instr[1] for instr in instructions if instr[0] == 'eqrr')
    return key_register, termination_value

def calculate_instructions(r0, termination_value):
    # This function simulates the core logic of the program
    # It's a simplified version; the actual logic would be based on program analysis
    r1 = 0
    instructions = 0
    while r1 != termination_value:
        r1 = r0 | 65536
        r0 = 7571367
        while True:
            r0 = ((r0 + (r1 & 255)) & 16777215 * 65899) & 16777215
            if 256 > r1:
                instructions += 1
                break
            r1 //= 256
        instructions += 1
    return instructions

def solve(instructions):
    key_register, termination_value = analyze_program(instructions)
    
    # Part 1: Find the lowest value that halts the program
    r0 = 0
    while calculate_instructions(r0, termination_value) > 0:
        r0 += 1
    part1 = r0 - 1
    
    # Part 2: Find the highest value that halts the program
    seen = set()
    r0 = 0
    while r0 not in seen:
        seen.add(r0)
        r0 = calculate_instructions(r0, termination_value)
    part2 = max(seen)
    
    return part1, part2

# Example usage
instructions = [
    # ... (insert actual instructions here)
]
part1, part2 = solve(instructions)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
