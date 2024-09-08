def alu_operation(operation, a, b, variables):
    if operation == 'inp':
        variables[a] = int(b)
    elif operation == 'add':
        variables[a] += variables[b] if b in variables else int(b)
    elif operation == 'mul':
        variables[a] *= variables[b] if b in variables else int(b)
    elif operation == 'div':
        variables[a] //= variables[b] if b in variables else int(b)
    elif operation == 'mod':
        variables[a] %= variables[b] if b in variables else int(b)
    elif operation == 'eql':
        variables[a] = 1 if variables[a] == (variables[b] if b in variables else int(b)) else 0

def run_monad(instructions, model_number):
    variables = {'w': 0, 'x': 0, 'y': 0, 'z': 0}
    input_index = 0
    for instruction in instructions:
        parts = instruction.split()
        operation = parts[0]
        if operation == 'inp':
            if input_index >= len(model_number):
                return None  # Invalid model number (too short)
            alu_operation(operation, parts[1], model_number[input_index], variables)
            input_index += 1
        else:
            alu_operation(operation, parts[1], parts[2], variables)
    return variables['z']

def find_valid_model_numbers(instructions):
    valid_numbers = []
    for num in range(11111111111111, 99999999999999 + 1):
        model_number = str(num)
        if '0' not in model_number:
            result = run_monad(instructions, model_number)
            if result == 0:
                valid_numbers.append(num)
    return valid_numbers

def solve_puzzle(instructions):
    valid_numbers = find_valid_model_numbers(instructions)
    if not valid_numbers:
        return "No valid model numbers found."
    largest = max(valid_numbers)
    smallest = min(valid_numbers)
    return f"Largest valid model number: {largest}\nSmallest valid model number: {smallest}"

# Read instructions from file
with open('input.txt', 'r') as file:
    instructions = file.read().strip().split('\n')

print(solve_puzzle(instructions))
