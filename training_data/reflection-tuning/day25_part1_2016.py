def parse_instructions(filename):
    with open(filename, 'r') as file:
        return [line.strip().split() for line in file]

def execute(instructions, a):
    registers = {'a': a, 'b': 0, 'c': 0, 'd': 0}
    pc = 0
    
    def get_value(x):
        return registers[x] if x in registers else int(x)
    
    while pc < len(instructions):
        inst = instructions[pc]
        if inst[0] == 'cpy':
            registers[inst[2]] = get_value(inst[1])
        elif inst[0] == 'inc':
            registers[inst[1]] += 1
        elif inst[0] == 'dec':
            registers[inst[1]] -= 1
        elif inst[0] == 'jnz':
            if get_value(inst[1]) != 0:
                pc += get_value(inst[2]) - 1
        elif inst[0] == 'out':
            yield get_value(inst[1])
        pc += 1

def is_clock_signal(generator):
    expected = 0
    for _ in range(100):  # Check first 100 outputs
        try:
            if next(generator) != expected:
                return False
            expected = 1 - expected  # Toggle between 0 and 1
        except StopIteration:
            return False
    return True

def find_lowest_integer(instructions):
    a = 0
    while True:
        if is_clock_signal(execute(instructions, a)):
            return a
        a += 1

instructions = parse_instructions('input.txt')
result = find_lowest_integer(instructions)
print(result)
