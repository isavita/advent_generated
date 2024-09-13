import sys
from collections import defaultdict
from typing import List, Tuple, Dict

class IntcodeComputer:
    def __init__(self, program: List[int]):
        self.memory = defaultdict(int)  # Extendable memory
        for i, val in enumerate(program):
            self.memory[i] = val
        self.ip = 0  # Instruction pointer
        self.relative_base = 0
        self.halted = False

    def get_parameter(self, mode: int, offset: int) -> int:
        """Retrieve the parameter based on its mode."""
        param = self.memory[self.ip + offset]
        if mode == 0:  # Position mode
            return self.memory[param]
        elif mode == 1:  # Immediate mode
            return param
        elif mode == 2:  # Relative mode
            return self.memory[self.relative_base + param]
        else:
            raise ValueError(f"Unknown parameter mode: {mode}")

    def set_parameter(self, mode: int, offset: int, value: int):
        """Set the parameter based on its mode."""
        param = self.memory[self.ip + offset]
        if mode == 0:  # Position mode
            self.memory[param] = value
        elif mode == 2:  # Relative mode
            self.memory[self.relative_base + param] = value
        else:
            raise ValueError(f"Unknown parameter mode for writing: {mode}")

    def run(self):
        """Generator to run the Intcode program."""
        while True:
            instruction = self.memory[self.ip]
            opcode = instruction % 100
            modes = [
                (instruction // 100) % 10,
                (instruction // 1000) % 10,
                (instruction // 10000) % 10
            ]

            if opcode == 1:  # Addition
                param1 = self.get_parameter(modes[0], 1)
                param2 = self.get_parameter(modes[1], 2)
                self.set_parameter(modes[2], 3, param1 + param2)
                self.ip += 4

            elif opcode == 2:  # Multiplication
                param1 = self.get_parameter(modes[0], 1)
                param2 = self.get_parameter(modes[1], 2)
                self.set_parameter(modes[2], 3, param1 * param2)
                self.ip += 4

            elif opcode == 3:  # Input
                # Yield a request for input
                input_val = yield 'need_input'
                self.set_parameter(modes[0], 1, input_val)
                self.ip += 2

            elif opcode == 4:  # Output
                output_val = self.get_parameter(modes[0], 1)
                self.ip += 2
                yield output_val

            elif opcode == 5:  # Jump-if-true
                param1 = self.get_parameter(modes[0], 1)
                param2 = self.get_parameter(modes[1], 2)
                if param1 != 0:
                    self.ip = param2
                else:
                    self.ip += 3

            elif opcode == 6:  # Jump-if-false
                param1 = self.get_parameter(modes[0], 1)
                param2 = self.get_parameter(modes[1], 2)
                if param1 == 0:
                    self.ip = param2
                else:
                    self.ip += 3

            elif opcode == 7:  # Less than
                param1 = self.get_parameter(modes[0], 1)
                param2 = self.get_parameter(modes[1], 2)
                self.set_parameter(modes[2], 3, int(param1 < param2))
                self.ip += 4

            elif opcode == 8:  # Equals
                param1 = self.get_parameter(modes[0], 1)
                param2 = self.get_parameter(modes[1], 2)
                self.set_parameter(modes[2], 3, int(param1 == param2))
                self.ip += 4

            elif opcode == 9:  # Adjust relative base
                param1 = self.get_parameter(modes[0], 1)
                self.relative_base += param1
                self.ip += 2

            elif opcode == 99:  # Halt
                self.halted = True
                break

            else:
                raise ValueError(f"Unknown opcode: {opcode}")

class Robot:
    def __init__(self, program: List[int], start_panel_color: int = 0):
        self.computer = IntcodeComputer(program)
        self.generator = self.computer.run()
        self.direction = 0  # 0: Up, 1: Right, 2: Down, 3: Left
        self.position = (0, 0)  # (x, y)
        self.panels: Dict[Tuple[int, int], int] = defaultdict(int)
        self.panels[self.position] = start_panel_color
        self.painted_panels = set()

    def turn_and_move(self, turn_direction: int):
        """Turn the robot and move forward one panel."""
        if turn_direction == 0:  # Turn left
            self.direction = (self.direction - 1) % 4
        elif turn_direction == 1:  # Turn right
            self.direction = (self.direction + 1) % 4
        else:
            raise ValueError(f"Unknown turn direction: {turn_direction}")

        # Move forward
        x, y = self.position
        if self.direction == 0:  # Up
            self.position = (x, y - 1)
        elif self.direction == 1:  # Right
            self.position = (x + 1, y)
        elif self.direction == 2:  # Down
            self.position = (x, y + 1)
        elif self.direction == 3:  # Left
            self.position = (x - 1, y)

    def run(self):
        """Run the robot until the Intcode program halts."""
        try:
            # Initialize the generator and advance to the first yield
            next_step = next(self.generator)
            while True:
                if next_step == 'need_input':
                    # Get current panel color
                    current_color = self.panels[self.position]
                    # Send the current color as input
                    try:
                        next_step = self.generator.send(current_color)
                    except StopIteration:
                        break
                    continue
                else:
                    # Receive paint instruction
                    paint_color = next_step
                    # Get the next output for turn direction
                    try:
                        turn_direction = next(self.generator)
                    except StopIteration:
                        break
                    # Paint the current panel
                    self.panels[self.position] = paint_color
                    self.painted_panels.add(self.position)
                    # Turn and move
                    self.turn_and_move(turn_direction)

                    # Get the next output or input request
                    try:
                        next_step = next(self.generator)
                    except StopIteration:
                        break
        except StopIteration:
            pass

    def get_painted_panels_count(self) -> int:
        """Return the number of panels painted at least once."""
        return len(self.painted_panels)

    def render_panels(self):
        """Render the painted panels to visualize the registration identifier."""
        if not self.panels:
            print("No panels painted.")
            return

        # Determine the bounds
        min_x = min(x for x, y in self.panels.keys())
        max_x = max(x for x, y in self.panels.keys())
        min_y = min(y for x, y in self.panels.keys())
        max_y = max(y for x, y in self.panels.keys())

        # Create the grid
        grid = []
        for y in range(min_y, max_y + 1):
            row = []
            for x in range(min_x, max_x + 1):
                if self.panels.get((x, y), 0) == 1:
                    row.append('#')
                else:
                    row.append(' ')
            grid.append(''.join(row))

        # Print the grid
        print("\nRegistration Identifier:")
        for line in grid:
            print(line)

def parse_input(file_path: str) -> List[int]:
    """Parse the Intcode program from the input file."""
    with open(file_path, 'r') as file:
        content = file.read().strip()
    return [int(x) for x in content.split(',')]

def main():
    input_file = 'input.txt'
    program = parse_input(input_file)

    # Part One
    robot_part1 = Robot(program, start_panel_color=0)
    robot_part1.run()
    painted_count_part1 = robot_part1.get_painted_panels_count()
    print(f"Part One: {painted_count_part1} panels painted at least once.")

    # Part Two
    robot_part2 = Robot(program, start_panel_color=1)
    robot_part2.run()
    print(f"Part Two: Registration identifier painted on the hull.")
    robot_part2.render_panels()

if __name__ == "__main__":
    main()
