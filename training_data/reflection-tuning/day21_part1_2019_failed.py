from collections import defaultdict

class IntcodeComputer:
    def __init__(self, program):
        self.memory = defaultdict(int, enumerate(map(int, program.split(','))))
        self.ip = 0
        self.relative_base = 0

    def get_param(self, mode, value):
        if mode == 0:
            return self.memory[value]
        elif mode == 1:
            return value
        elif mode == 2:
            return self.memory[self.relative_base + value]

    def set_param(self, mode, addr, value):
        if mode == 0:
            self.memory[addr] = value
        elif mode == 2:
            self.memory[self.relative_base + addr] = value

    def run(self, input_value):
        while True:
            opcode = self.memory[self.ip] % 100
            modes = [int(d) for d in f"{self.memory[self.ip]:05d}"[2::-1]]

            if opcode == 99:
                return None

            a = self.memory[self.ip + 1]
            b = self.memory[self.ip + 2]
            c = self.memory[self.ip + 3]

            if opcode == 1:
                result = self.get_param(modes[0], a) + self.get_param(modes[1], b)
                self.set_param(modes[2], c, result)
                self.ip += 4
            elif opcode == 2:
                result = self.get_param(modes[0], a) * self.get_param(modes[1], b)
                self.set_param(modes[2], c, result)
                self.ip += 4
            elif opcode == 3:
                self.set_param(modes[0], a, input_value)
                self.ip += 2
            elif opcode == 4:
                output = self.get_param(modes[0], a)
                self.ip += 2
                return output
            elif opcode == 5:
                if self.get_param(modes[0], a) != 0:
                    self.ip = self.get_param(modes[1], b)
                else:
                    self.ip += 3
            elif opcode == 6:
                if self.get_param(modes[0], a) == 0:
                    self.ip = self.get_param(modes[1], b)
                else:
                    self.ip += 3
            elif opcode == 7:
                result = int(self.get_param(modes[0], a) < self.get_param(modes[1], b))
                self.set_param(modes[2], c, result)
                self.ip += 4
            elif opcode == 8:
                result = int(self.get_param(modes[0], a) == self.get_param(modes[1], b))
                self.set_param(modes[2], c, result)
                self.ip += 4
            elif opcode == 9:
                self.relative_base += self.get_param(modes[0], a)
                self.ip += 2

def solve(program):
    computer = IntcodeComputer(program)
    
    # Springscript program
    script = """
    NOT A J
    NOT B T
    OR T J
    NOT C T
    OR T J
    AND D J
    WALK
    """
    
    for line in script.strip().split('\n'):
        for char in line.strip():
            output = computer.run(ord(char))
            if output is not None and output > 127:
                return output
        output = computer.run(10)  # newline
        if output is not None and output > 127:
            return output
    
    while True:
        output = computer.run(0)
        if output is None:
            break
        if output > 127:
            return output
        print(chr(output), end='')

# Read the Intcode program from a file named 'input.txt'
with open('input.txt', 'r') as file:
    program = file.read().strip()

result = solve(program)
print(f"Hull damage reported: {result}")
