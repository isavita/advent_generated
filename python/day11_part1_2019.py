
import sys

class Intcode:
    def __init__(self, program):
        self.memory = program[:]
        self.ip = 0
        self.input = []
        self.output = []
        self.halted = False

    def add_input(self, input):
        self.input.append(input)

    def run(self):
        self.output = []
        while True:
            opcode = self.memory[self.ip] % 100
            if opcode in [1, 2, 7, 8]:
                self.ensure_memory(self.ip + 3)
                params = self.get_params(3)
                val1, val2 = self.read_memory(params[0]), self.read_memory(params[1])
                if opcode == 1:
                    self.write_memory(params[2], val1 + val2)
                elif opcode == 2:
                    self.write_memory(params[2], val1 * val2)
                elif opcode == 7 and val1 < val2 or opcode == 8 and val1 == val2:
                    self.write_memory(params[2], 1)
                else:
                    self.write_memory(params[2], 0)
                self.ip += 4
            elif opcode in [3, 4]:
                self.ensure_memory(self.ip + 1)
                params = self.get_params(1)
                if opcode == 3:
                    if not self.input:
                        return
                    self.write_memory(params[0], self.input.pop(0))
                else:
                    self.output.append(self.read_memory(params[0]))
                self.ip += 2
            elif opcode in [5, 6]:
                self.ensure_memory(self.ip + 2)
                params = self.get_params(2)
                val, target = self.read_memory(params[0]), self.read_memory(params[1])
                if (opcode == 5 and val != 0) or (opcode == 6 and val == 0):
                    self.ip = target
                else:
                    self.ip += 3
            elif opcode == 99:
                self.halted = True
                return
            else:
                raise Exception(f"Unknown opcode: {opcode}")

    def read_memory(self, address):
        self.ensure_memory(address)
        return self.memory[address]

    def write_memory(self, address, value):
        self.ensure_memory(address)
        self.memory[address] = value

    def ensure_memory(self, address):
        if address >= len(self.memory):
            self.memory.extend([0] * (address + 1 - len(self.memory)))

    def get_params(self, count):
        param_modes = self.memory[self.ip] // 100
        params = []
        for i in range(count):
            if param_modes % 10 == 1:
                params.append(self.ip + i + 1)
            else:
                params.append(self.memory[self.ip + i + 1])
            param_modes //= 10
        return params

    def outputs(self):
        return self.output

    def halted(self):
        return self.halted

class Robot:
    def __init__(self):
        self.position = (0, 0)
        self.direction = 0  # 0: Up, 1: Right, 2: Down, 3: Left

    def turn_and_move(self, turn_direction):
        if turn_direction == 0:
            self.direction = (self.direction - 1) % 4  # Turn left
        else:
            self.direction = (self.direction + 1) % 4  # Turn right

        if self.direction == 0:
            self.position = (self.position[0], self.position[1] - 1)
        elif self.direction == 1:
            self.position = (self.position[0] + 1, self.position[1])
        elif self.direction == 2:
            self.position = (self.position[0], self.position[1] + 1)
        else:
            self.position = (self.position[0] - 1, self.position[1])

def main():
    with open("input.txt") as f:
        program = list(map(int, f.read().strip().split(',')))

    grid = {}
    robot = Robot()
    intcode = Intcode(program)

    while not intcode.halted:
        current_color = grid.get(robot.position, 0)
        intcode.add_input(current_color)
        intcode.run()
        outputs = intcode.outputs()

        if len(outputs) == 2:
            grid[robot.position] = outputs[0]
            robot.turn_and_move(outputs[1])

    print(len(grid))

if __name__ == "__main__":
    main()
