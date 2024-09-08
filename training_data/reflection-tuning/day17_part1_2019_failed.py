from intcode import IntcodeComputer

def run_ascii_program(program):
    computer = IntcodeComputer(program)
    output = computer.run()
    return ''.join(chr(x) for x in output if x < 128)

def parse_scaffold(ascii_output):
    return [list(line) for line in ascii_output.strip().split('\n')]

def find_intersections(grid):
    intersections = []
    for y in range(1, len(grid) - 1):
        for x in range(1, len(grid[y]) - 1):
            if (grid[y][x] == '#' and
                grid[y-1][x] == '#' and
                grid[y+1][x] == '#' and
                grid[y][x-1] == '#' and
                grid[y][x+1] == '#'):
                intersections.append((x, y))
    return intersections

def calculate_alignment_sum(intersections):
    return sum(x * y for x, y in intersections)

def solve(program):
    ascii_output = run_ascii_program(program)
    scaffold_grid = parse_scaffold(ascii_output)
    intersections = find_intersections(scaffold_grid)
    alignment_sum = calculate_alignment_sum(intersections)
    return alignment_sum

# Assuming the program input is stored in a variable called 'program'
result = solve(program)
print(result)
