import sys
from collections import defaultdict, deque

class IntcodeComputer:
    def __init__(self, memory):
        self.memory = defaultdict(int)
        for i, val in enumerate(memory):
            self.memory[i] = val
        self.pointer = 0
        self.relative_base = 0
        self.inputs = deque()
        self.outputs = deque()
        self.halted = False

    def get_param(self, mode, param):
        if mode == 0:
            # Position mode
            return self.memory[param]
        elif mode == 1:
            # Immediate mode
            return param
        elif mode == 2:
            # Relative mode
            return self.memory[self.relative_base + param]
        else:
            raise Exception(f"Unknown parameter mode: {mode}")

    def set_param(self, mode, param, value):
        if mode == 0:
            # Position mode
            self.memory[param] = value
        elif mode == 2:
            # Relative mode
            self.memory[self.relative_base + param] = value
        else:
            raise Exception(f"Unknown parameter mode for writing: {mode}")

    def run(self):
        while True:
            instruction = self.memory[self.pointer]
            opcode = instruction % 100
            modes = [
                (instruction // 100) % 10,
                (instruction // 1000) % 10,
                (instruction // 10000) % 10
            ]

            if opcode == 1:
                # Addition
                param1 = self.memory[self.pointer + 1]
                param2 = self.memory[self.pointer + 2]
                param3 = self.memory[self.pointer + 3]
                val1 = self.get_param(modes[0], param1)
                val2 = self.get_param(modes[1], param2)
                self.set_param(modes[2], param3, val1 + val2)
                self.pointer += 4
            elif opcode == 2:
                # Multiplication
                param1 = self.memory[self.pointer + 1]
                param2 = self.memory[self.pointer + 2]
                param3 = self.memory[self.pointer + 3]
                val1 = self.get_param(modes[0], param1)
                val2 = self.get_param(modes[1], param2)
                self.set_param(modes[2], param3, val1 * val2)
                self.pointer += 4
            elif opcode == 3:
                # Input
                if not self.inputs:
                    # Wait for input
                    return
                param1 = self.memory[self.pointer + 1]
                input_value = self.inputs.popleft()
                self.set_param(modes[0], param1, input_value)
                self.pointer += 2
            elif opcode == 4:
                # Output
                param1 = self.memory[self.pointer + 1]
                output_value = self.get_param(modes[0], param1)
                self.outputs.append(output_value)
                self.pointer += 2
            elif opcode == 5:
                # Jump-if-true
                param1 = self.memory[self.pointer + 1]
                param2 = self.memory[self.pointer + 2]
                val1 = self.get_param(modes[0], param1)
                val2 = self.get_param(modes[1], param2)
                if val1 != 0:
                    self.pointer = val2
                else:
                    self.pointer += 3
            elif opcode == 6:
                # Jump-if-false
                param1 = self.memory[self.pointer + 1]
                param2 = self.memory[self.pointer + 2]
                val1 = self.get_param(modes[0], param1)
                val2 = self.get_param(modes[1], param2)
                if val1 == 0:
                    self.pointer = val2
                else:
                    self.pointer += 3
            elif opcode == 7:
                # Less than
                param1 = self.memory[self.pointer + 1]
                param2 = self.memory[self.pointer + 2]
                param3 = self.memory[self.pointer + 3]
                val1 = self.get_param(modes[0], param1)
                val2 = self.get_param(modes[1], param2)
                self.set_param(modes[2], param3, int(val1 < val2))
                self.pointer += 4
            elif opcode == 8:
                # Equals
                param1 = self.memory[self.pointer + 1]
                param2 = self.memory[self.pointer + 2]
                param3 = self.memory[self.pointer + 3]
                val1 = self.get_param(modes[0], param1)
                val2 = self.get_param(modes[1], param2)
                self.set_param(modes[2], param3, int(val1 == val2))
                self.pointer += 4
            elif opcode == 9:
                # Adjust relative base
                param1 = self.memory[self.pointer + 1]
                val1 = self.get_param(modes[0], param1)
                self.relative_base += val1
                self.pointer += 2
            elif opcode == 99:
                # Halt
                self.halted = True
                return
            else:
                raise Exception(f"Unknown opcode: {opcode}")

def read_input(filename):
    with open(filename, 'r') as f:
        content = f.read().strip()
    return [int(x) for x in content.split(',')]

def parse_map(output):
    grid = []
    line = []
    for c in output:
        if c == 10:
            if line:
                grid.append(line)
                line = []
        else:
            line.append(chr(c))
    if line:
        grid.append(line)
    return grid

def find_intersections(grid):
    intersections = []
    for y in range(1, len(grid) -1):
        for x in range(1, len(grid[0]) -1):
            if grid[y][x] != '#':
                continue
            # Check all four adjacent positions
            if (grid[y-1][x] == '#' and
                grid[y+1][x] == '#' and
                grid[y][x-1] == '#' and
                grid[y][x+1] == '#'):
                intersections.append((x, y))
    return intersections

def find_robot_position(grid):
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell in ['^', 'v', '<', '>', 'X']:
                return (x, y, cell)
    return None

def turn_left(direction):
    # Directions: '^' (up), 'v' (down), '<' (left), '>' (right)
    return {'^': '<', '<': 'v', 'v': '>', '>': '^'}[direction]

def turn_right(direction):
    return {'^': '>', '>': 'v', 'v': '<', '<': '^'}[direction]

def move_forward(x, y, direction):
    if direction == '^':
        return (x, y -1)
    elif direction == 'v':
        return (x, y +1)
    elif direction == '<':
        return (x -1, y)
    elif direction == '>':
        return (x +1, y)
    else:
        raise Exception(f"Unknown direction: {direction}")

def get_movement_path(grid, start_x, start_y, start_dir):
    x, y, direction = start_x, start_y, start_dir
    path = []
    steps = 0
    while True:
        # Attempt to move forward
        next_x, next_y = move_forward(x, y, direction)
        if 0 <= next_y < len(grid) and 0 <= next_x < len(grid[0]) and grid[next_y][next_x] == '#':
            x, y = next_x, next_y
            steps +=1
        else:
            # Can't move forward, record steps if any
            if steps >0:
                path.append(str(steps))
                steps =0
            # Try turning left
            left_dir = turn_left(direction)
            next_x, next_y = move_forward(x, y, left_dir)
            if 0 <= next_y < len(grid) and 0 <= next_x < len(grid[0]) and grid[next_y][next_x] == '#':
                path.append('L')
                direction = left_dir
                continue
            # Try turning right
            right_dir = turn_right(direction)
            next_x, next_y = move_forward(x, y, right_dir)
            if 0 <= next_y < len(grid) and 0 <= next_x < len(grid[0]) and grid[next_y][next_x] == '#':
                path.append('R')
                direction = right_dir
                continue
            # No where to turn, end
            break
    return path

def compress_movement(path):
    from itertools import combinations

    def is_valid_routine(routine):
        return len(routine) <= 20

    def replace_sequence(seq, pattern, replacement):
        i = 0
        res = []
        while i < len(seq):
            if seq[i:i+len(pattern)] == pattern:
                res.append(replacement)
                i += len(pattern)
            else:
                res.append(seq[i])
                i += 1
        return res

    path_str = ','.join(path)
    tokens = path_str.split(',')

    # Generate possible patterns for A, B, C
    max_function_length = 20  # Max characters per function
    max_pattern_length = 10   # Max tokens per function (since 'L', 'R', and numbers)

    for a_len in range(1, max_pattern_length+1):
        a_pattern = tokens[:a_len]
        a_str = ','.join(a_pattern)
        if len(a_str) > max_function_length:
            continue
        tokens_after_a = replace_sequence(tokens, a_pattern, 'A')

        for b_start in range(a_len, len(tokens)):
            for b_len in range(1, max_pattern_length+1):
                b_end = b_start + b_len
                b_pattern = tokens[b_start:b_end]
                b_str = ','.join(b_pattern)
                if len(b_str) > max_function_length:
                    continue
                tokens_after_b = replace_sequence(tokens_after_a, b_pattern, 'B')

                for c_start in range(b_end, len(tokens)):
                    for c_len in range(1, max_pattern_length+1):
                        c_end = c_start + c_len
                        c_pattern = tokens[c_start:c_end]
                        c_str = ','.join(c_pattern)
                        if len(c_str) > max_function_length:
                            continue
                        tokens_after_c = replace_sequence(tokens_after_b, c_pattern, 'C')

                        # Now, replace any remaining sequences of a_pattern, b_pattern, or c_pattern
                        main_tokens = tokens_after_c.copy()
                        changed = True
                        while changed:
                            changed = False
                            temp_tokens = main_tokens.copy()
                            main_tokens = []
                            i = 0
                            while i < len(temp_tokens):
                                if temp_tokens[i:i+len(a_pattern)] == a_pattern:
                                    main_tokens.append('A')
                                    i += len(a_pattern)
                                    changed = True
                                elif temp_tokens[i:i+len(b_pattern)] == b_pattern:
                                    main_tokens.append('B')
                                    i += len(b_pattern)
                                    changed = True
                                elif temp_tokens[i:i+len(c_pattern)] == c_pattern:
                                    main_tokens.append('C')
                                    i += len(c_pattern)
                                    changed = True
                                else:
                                    main_tokens.append(temp_tokens[i])
                                    i += 1
                        main_routine = ','.join(main_tokens)
                        if all(c in 'ABC,' for c in main_routine) and len(main_routine) <= 20:
                            function_A = ','.join(a_pattern)
                            function_B = ','.join(b_pattern)
                            function_C = ','.join(c_pattern)
                            if len(function_A) <= 20 and len(function_B) <= 20 and len(function_C) <= 20:
                                return main_routine, function_A, function_B, function_C
    raise Exception("Could not compress the path into functions A, B, C.")

def main():
    # Read Intcode program from input.txt
    program = read_input('input.txt')

    # ----- Part One -----
    # Initialize Intcode Computer
    computer = IntcodeComputer(program.copy())
    computer.run()
    output = list(computer.outputs)
    # Parse the ASCII map
    grid = parse_map(output)
    # For debugging: print the map
    # for row in grid:
    #     print(''.join(row))
    # Find intersections
    intersections = find_intersections(grid)
    # Calculate alignment parameters
    alignment_sum = sum(x * y for (x, y) in intersections)
    print(f"Part One: Sum of alignment parameters = {alignment_sum}")

    # ----- Part Two -----
    # Modify the Intcode program: set address 0 to 2
    program_part2 = program.copy()
    program_part2[0] = 2
    computer_part2 = IntcodeComputer(program_part2)

    # Determine the robot's starting position and direction
    robot = find_robot_position(grid)
    if not robot:
        raise Exception("Robot not found on the scaffold.")
    start_x, start_y, start_dir = robot

    # Get the movement path
    movement_path = get_movement_path(grid, start_x, start_y, start_dir)
    # For debugging: print the movement path
    # print("Movement Path:", movement_path)

    # Compress the movement path into main routine and functions A, B, C
    try:
        main_routine, function_A, function_B, function_C = compress_movement(movement_path)
    except Exception as e:
        print("Error in compressing path:", e)
        sys.exit(1)

    # Prepare the input for the Intcode program
    # Each line must end with a newline character (ASCII 10)
    # The final input is 'n' followed by newline
    input_lines = [
        main_routine,
        function_A,
        function_B,
        function_C,
        'n'
    ]
    movement_inputs = []
    for line in input_lines:
        for char in line:
            movement_inputs.append(ord(char))
        movement_inputs.append(10)  # Newline

    # Feed the movement inputs to the Intcode computer
    computer_part2.inputs = deque(movement_inputs)

    # Run the Intcode computer
    while not computer_part2.halted:
        computer_part2.run()

    # Capture the final output (dust collected)
    # The dust value is the last output value and is a large integer
    dust_collected = 0
    while computer_part2.outputs:
        dust_collected = computer_part2.outputs.popleft()

    print(f"Part Two: Dust collected = {dust_collected}")

if __name__ == "__main__":
    main()
