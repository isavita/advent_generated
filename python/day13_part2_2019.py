from collections import defaultdict

class IntcodeComputer:
    def __init__(self, program):
        self.memory = defaultdict(int, {i: program[i] for i in range(len(program))})
        self.ip = 0
        self.relative_base = 0
        self.output = []
        self.awaiting_input = False

    def get_parameter(self, offset, mode):
        if mode == 0:  # Position mode
            return self.memory[self.memory[self.ip + offset]]
        elif mode == 1:  # Immediate mode
            return self.memory[self.ip + offset]
        elif mode == 2:  # Relative mode
            return self.memory[self.relative_base + self.memory[self.ip + offset]]

    def get_write_address(self, offset, mode):
        if mode == 0:  # Position mode
            return self.memory[self.ip + offset]
        elif mode == 2:  # Relative mode
            return self.relative_base + self.memory[self.ip + offset]

    def execute(self, input_provider):
        while True:
            instruction = self.memory[self.ip]
            opcode = instruction % 100
            modes = [(instruction // 10 ** i) % 10 for i in range(2, 5)]

            if opcode == 99:  # Program complete
                break

            if opcode == 3:  # Input
                if not self.awaiting_input:
                    input_value = input_provider()
                    self.memory[self.get_write_address(1, modes[0])] = input_value
                    self.awaiting_input = True
                self.ip += 2
            elif opcode == 4:  # Output
                output_value = self.get_parameter(1, modes[0])
                self.ip += 2
                self.awaiting_input = False
                yield output_value
            else:
                self.perform_operation(opcode, modes)

    def perform_operation(self, opcode, modes):
        param1 = self.get_parameter(1, modes[0])
        param2 = self.get_parameter(2, modes[1]) if opcode not in [3, 4, 9] else None

        if opcode in [1, 2]:  # Add or multiply
            result = param1 + param2 if opcode == 1 else param1 * param2
            self.memory[self.get_write_address(3, modes[2])] = result
            self.ip += 4
        elif opcode in [5, 6]:  # Jump-if-true or jump-if-false
            if (opcode == 5 and param1 != 0) or (opcode == 6 and param1 == 0):
                self.ip = param2
            else:
                self.ip += 3
        elif opcode in [7, 8]:  # Less than or equals
            self.memory[self.get_write_address(3, modes[2])] = int((opcode == 7 and param1 < param2) or (opcode == 8 and param1 == param2))
            self.ip += 4
        elif opcode == 9:  # Adjust relative base
            self.relative_base += param1
            self.ip += 2

def parse_input(file_path):
    with open(file_path, 'r') as file:
        return [int(n) for n in file.read().strip().split(',')]

def play_game(program):
    computer = IntcodeComputer(program)
    computer.memory[0] = 2  # Set to free play mode
    outputs = computer.execute(lambda: (ball_x > paddle_x) - (ball_x < paddle_x))
    score, ball_x, paddle_x = 0, 0, 0

    try:
        while True:
            x = next(outputs)
            y = next(outputs)
            tile_id = next(outputs)
            if x == -1 and y == 0:
                score = tile_id
            else:
                if tile_id == 3:
                    paddle_x = x
                elif tile_id == 4:
                    ball_x = x
    except StopIteration:
        pass  # Game has completed

    return score

program = parse_input('input.txt')
print("Final Score:", play_game(program))
