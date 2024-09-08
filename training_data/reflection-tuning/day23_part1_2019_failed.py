from collections import deque
import itertools

class IntcodeComputer:
    def __init__(self, program, input_func, output_func):
        self.memory = list(program)
        self.ip = 0
        self.relative_base = 0
        self.input_func = input_func
        self.output_func = output_func

    def run(self):
        while True:
            opcode = self.memory[self.ip] % 100
            if opcode == 99:
                return
            modes = [int(x) for x in f"{self.memory[self.ip]:05}"[2::-1]]
            params = [self.get_param(i, mode) for i, mode in enumerate(modes, start=1)]
            
            if opcode == 1:
                self.memory[params[2]] = self.memory[params[0]] + self.memory[params[1]]
                self.ip += 4
            elif opcode == 2:
                self.memory[params[2]] = self.memory[params[0]] * self.memory[params[1]]
                self.ip += 4
            elif opcode == 3:
                self.memory[params[0]] = self.input_func()
                self.ip += 2
            elif opcode == 4:
                self.output_func(self.memory[params[0]])
                self.ip += 2
            elif opcode == 5:
                self.ip = self.memory[params[1]] if self.memory[params[0]] != 0 else self.ip + 3
            elif opcode == 6:
                self.ip = self.memory[params[1]] if self.memory[params[0]] == 0 else self.ip + 3
            elif opcode == 7:
                self.memory[params[2]] = int(self.memory[params[0]] < self.memory[params[1]])
                self.ip += 4
            elif opcode == 8:
                self.memory[params[2]] = int(self.memory[params[0]] == self.memory[params[1]])
                self.ip += 4
            elif opcode == 9:
                self.relative_base += self.memory[params[0]]
                self.ip += 2

    def get_param(self, offset, mode):
        if mode == 0:
            return self.memory[self.ip + offset]
        elif mode == 1:
            return self.ip + offset
        elif mode == 2:
            return self.relative_base + self.memory[self.ip + offset]

class Network:
    def __init__(self, program):
        self.computers = []
        self.queues = [deque() for _ in range(50)]
        self.packet_255 = None

        for address in range(50):
            input_queue = iter(itertools.chain([address], self.input_generator(address)))
            computer = IntcodeComputer(program, lambda: next(input_queue), self.output_handler(address))
            self.computers.append(computer)

    def input_generator(self, address):
        while True:
            if self.queues[address]:
                yield self.queues[address].popleft()
            else:
                yield -1

    def output_handler(self, address):
        output_buffer = []
        def handle_output(value):
            output_buffer.append(value)
            if len(output_buffer) == 3:
                dest, x, y = output_buffer
                if dest == 255:
                    self.packet_255 = (x, y)
                else:
                    self.queues[dest].extend([x, y])
                output_buffer.clear()
        return handle_output

    def run(self):
        while not self.packet_255:
            for computer in self.computers:
                computer.run()
        return self.packet_255[1]

def solve(program):
    network = Network(program)
    return network.run()

if __name__ == "__main__":
    with open("input.txt", "r") as f:
        program = list(map(int, f.read().strip().split(",")))
    
    result = solve(program)
    print(result)
