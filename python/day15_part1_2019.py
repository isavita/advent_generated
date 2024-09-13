import sys
from collections import defaultdict, deque
from typing import List, Tuple, Dict, Optional

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

class Droid:
    def __init__(self, program: List[int]):
        self.computer = IntcodeComputer(program)
        self.generator = self.computer.run()
        self.direction_map = {
            1: (0, -1),  # North
            2: (0, 1),   # South
            3: (-1, 0),  # West
            4: (1, 0)    # East
        }
        self.current_position = (0, 0)
        self.grid = defaultdict(int)  # (x, y) -> status (0: wall, 1: open, 2: oxygen)
        self.grid[self.current_position] = 1  # Starting position is open
        self.oxygen_position: Optional[Tuple[int, int]] = None
        self.path_length = 0

    def send_move_command(self, direction: int) -> int:
        """
        Sends a movement command to the Intcode computer and returns the status code.
        """
        try:
            # Request input
            status = next(self.generator)
            if status != 'need_input':
                raise ValueError("Expected 'need_input' but got something else.")
            # Send the movement command
            status = self.generator.send(direction)
            if status not in [0, 1, 2]:
                raise ValueError(f"Unexpected status code: {status}")
            return status
        except StopIteration:
            raise RuntimeError("Intcode program halted unexpectedly.")

    def explore(self):
        """
        Explores the grid using BFS to find the oxygen system.
        Returns the number of steps required to reach the oxygen system.
        """
        queue = deque()
        queue.append((self.current_position, 0))
        visited = set()
        visited.add(self.current_position)

        while queue:
            position, steps = queue.popleft()
            self.move_to(position)
            for direction in [1, 2, 3, 4]:  # North, South, West, East
                dx, dy = self.direction_map[direction]
                new_pos = (position[0] + dx, position[1] + dy)
                if new_pos in visited:
                    continue
                status = self.send_move_command(direction)
                if status == 0:
                    # Hit a wall
                    self.grid[new_pos] = 0
                elif status in [1, 2]:
                    # Moved to an open space or found oxygen system
                    self.grid[new_pos] = 1 if status == 1 else 2
                    visited.add(new_pos)
                    queue.append((new_pos, steps + 1))
                    if status == 2:
                        # Oxygen system found
                        return steps + 1
                    # Move back to the original position
                    opposite_direction = self.get_opposite_direction(direction)
                    self.send_move_command(opposite_direction)
        return -1  # Oxygen system not found

    def get_opposite_direction(self, direction: int) -> int:
        """
        Returns the opposite direction.
        """
        opposites = {1: 2, 2: 1, 3: 4, 4: 3}
        return opposites[direction]

    def move_to(self, target: Tuple[int, int]):
        """
        Moves the droid from its current position to the target position.
        Assumes that a path exists.
        """
        path = self.find_path(self.current_position, target)
        for direction in path:
            status = self.send_move_command(direction)
            if status == 0:
                raise RuntimeError(f"Unexpected wall while moving to {target}")
            dx, dy = self.direction_map[direction]
            self.current_position = (self.current_position[0] + dx, self.current_position[1] + dy)
            if status == 2:
                # Oxygen system found during movement
                self.oxygen_position = self.current_position

    def find_path(self, start: Tuple[int, int], end: Tuple[int, int]) -> List[int]:
        """
        Finds a path from start to end using BFS.
        Returns a list of movement commands.
        """
        queue = deque()
        queue.append((start, []))
        visited = set()
        visited.add(start)

        while queue:
            position, path = queue.popleft()
            if position == end:
                return path
            for direction in [1, 2, 3, 4]:
                dx, dy = self.direction_map[direction]
                new_pos = (position[0] + dx, position[1] + dy)
                if new_pos in visited or self.grid.get(new_pos, 0) == 0:
                    continue
                visited.add(new_pos)
                queue.append((new_pos, path + [direction]))
        raise ValueError(f"No path found from {start} to {end}.")

def parse_input(file_path: str) -> List[int]:
    """Parse the Intcode program from the input file."""
    with open(file_path, 'r') as file:
        content = file.read().strip()
    return [int(x) for x in content.split(',')]

def main():
    input_file = 'input.txt'
    program = parse_input(input_file)
    droid = Droid(program)
    steps = droid.explore()
    if steps != -1:
        print(f"Fewest number of movement commands to reach the oxygen system: {steps}")
    else:
        print("Oxygen system not found.")

if __name__ == "__main__":
    main()
