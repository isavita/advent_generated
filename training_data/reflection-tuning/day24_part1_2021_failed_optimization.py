def parse_program(program):
    blocks = []
    for line in program.strip().split('\n'):
        if line.startswith('inp'):
            blocks.append([])
        blocks[-1].append(line.split())
    return blocks

def analyze_blocks(blocks):
    constraints = []
    stack = []
    for i, block in enumerate(blocks):
        if block[4][2] == '1':  # This is a 'div z 1' block, which pushes to the stack
            stack.append((i, int(block[15][2])))  # Store index and the add x <val>
        else:  # This is a 'div z 26' block, which pops from the stack
            j, val = stack.pop()
            diff = int(block[5][2]) + val  # Calculate the difference between digits
            constraints.append((j, i, diff))
    return constraints

def find_largest_model_number(constraints):
    digits = [0] * 14
    for i, j, diff in constraints:
        if diff > 0:
            digits[i] = 9
            digits[j] = 9 - diff
        else:
            digits[i] = 9 + diff
            digits[j] = 9
    return ''.join(map(str, digits))

def solve(program):
    blocks = parse_program(program)
    constraints = analyze_blocks(blocks)
    return find_largest_model_number(constraints)

# Example usage:
# program = """
# inp w
# mul x 0
# add x z
# mod x 26
# div z 1
# add x 10
# eql x w
# eql x 0
# mul y 0
# add y 25
# mul y x
# add y 1
# mul z y
# mul y 0
# add y w
# add y 10
# mul y x
# add z y
# ...
# """
# print(solve(program))
