import sys
from collections import defaultdict

class IntcodeComputer:
    def __init__(self, memory):
        self.memory = defaultdict(int, enumerate(memory))
        self.ip = 0
        self.relative_base = 0
        self.input_buffer = []
        self.output_buffer = []

    def run(self):
        while True:
            opcode = self.memory[self.ip] % 100
            modes = [int(d) for d in f"{self.memory[self.ip]:05}"[:3]][::-1]

            if opcode == 99:
                return None

            params = [self.get_param(i + 1, mode) for i, mode in enumerate(modes)]

            if opcode == 1:
                self.memory[params[2]] = params[0] + params[1]
                self.ip += 4
            elif opcode == 2:
                self.memory[params[2]] = params[0] * params[1]
                self.ip += 4
            elif opcode == 3:
                if not self.input_buffer:
                    return "INPUT"
                self.memory[params[0]] = self.input_buffer.pop(0)
                self.ip += 2
            elif opcode == 4:
                self.output_buffer.append(params[0])
                self.ip += 2
                if len(self.output_buffer) == 1:
                    return "OUTPUT"
            elif opcode == 5:
                self.ip = params[1] if params[0] != 0 else self.ip + 3
            elif opcode == 6:
                self.ip = params[1] if params[0] == 0 else self.ip + 3
            elif opcode == 7:
                self.memory[params[2]] = 1 if params[0] < params[1] else 0
                self.ip += 4
            elif opcode == 8:
                self.memory[params[2]] = 1 if params[0] == params[1] else 0
                self.ip += 4
            elif opcode == 9:
                self.relative_base += params[0]
                self.ip += 2

    def get_param(self, offset, mode):
        if mode == 0:
            return self.memory[self.memory[self.ip + offset]]
        elif mode == 1:
            return self.memory[self.ip + offset]
        elif mode == 2:
            return self.memory[self.relative_base + self.memory[self.ip + offset]]

    def add_input(self, value):
        self.input_buffer.append(value)

    def get_output(self):
        return self.output_buffer.pop(0) if self.output_buffer else None

def main():
    with open("input.txt", "r") as f:
        program = list(map(int, f.read().strip().split(",")))

    computer = IntcodeComputer(program)
    output = ""

    while True:
        result = computer.run()

        if result == "OUTPUT":
            char = chr(computer.get_output())
            output += char
            print(char, end="")
            
            if output.endswith("Command?\n"):
                command = input().strip()
                for c in command + "\n":
                    computer.add_input(ord(c))
                output = ""
        elif result == "INPUT":
            # This should not happen as we always provide input after "Command?"
            print("Unexpected input request")
            break
        elif result is None:
            print("Program halted")
            break

if __name__ == "__main__":
    main()
