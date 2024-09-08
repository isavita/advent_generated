class Computer:
    def __init__(self, program):
        self.memory = list(program)
        self.ip = 0
        self.relative_base = 0
        self.inputs = []
        self.outputs = []

    def get_mem(self, address):
        if address >= len(self.memory):
            self.memory.extend([0] * (address - len(self.memory) + 1))
        return self.memory[address]

    def set_mem(self, address, value):
        if address >= len(self.memory):
            self.memory.extend([0] * (address - len(self.memory) + 1))
        self.memory[address] = value

    def get_param(self, param, mode):
        if mode == 0:
            return self.get_mem(param)
        elif mode == 1:
            return param
        elif mode == 2:
            return self.get_mem(self.relative_base + param)

    def run(self):
        while True:
            opcode = self.memory[self.ip] % 100
            modes = [int(x) for x in str(self.memory[self.ip] // 100).zfill(3)[::-1]]

            if opcode == 99:
                return "HALT"

            if opcode in [1, 2, 7, 8]:
                a = self.get_param(self.get_mem(self.ip + 1), modes[0])
                b = self.get_param(self.get_mem(self.ip + 2), modes[1])
                c = self.get_mem(self.ip + 3) + (self.relative_base if modes[2] == 2 else 0)
                if opcode == 1:
                    self.set_mem(c, a + b)
                elif opcode == 2:
                    self.set_mem(c, a * b)
                elif opcode == 7:
                    self.set_mem(c, 1 if a < b else 0)
                elif opcode == 8:
                    self.set_mem(c, 1 if a == b else 0)
                self.ip += 4
            elif opcode in [3, 4]:
                a = self.get_mem(self.ip + 1) + (self.relative_base if modes[0] == 2 else 0)
                if opcode == 3:
                    if not self.inputs:
                        return "WAIT_INPUT"
                    self.set_mem(a, self.inputs.pop(0))
                else:
                    self.outputs.append(self.get_param(a, modes[0]))
                self.ip += 2
            elif opcode in [5, 6]:
                a = self.get_param(self.get_mem(self.ip + 1), modes[0])
                b = self.get_param(self.get_mem(self.ip + 2), modes[1])
                if (opcode == 5 and a != 0) or (opcode == 6 and a == 0):
                    self.ip = b
                else:
                    self.ip += 3
            elif opcode == 9:
                self.relative_base += self.get_param(self.get_mem(self.ip + 1), modes[0])
                self.ip += 2

class Network:
    def __init__(self, program):
        self.computers = [Computer(program) for _ in range(50)]
        self.queues = [[] for _ in range(50)]

    def run(self):
        for i, computer in enumerate(self.computers):
            computer.inputs.append(i)

        while True:
            for i, computer in enumerate(self.computers):
                if not computer.inputs:
                    computer.inputs.append(-1)
                
                status = computer.run()
                
                while len(computer.outputs) >= 3:
                    addr, x, y = computer.outputs[:3]
                    del computer.outputs[:3]
                    
                    if addr == 255:
                        return y
                    
                    self.queues[addr].append((x, y))
                
                if self.queues[i]:
                    x, y = self.queues[i].pop(0)
                    computer.inputs.extend([x, y])

def solve(program):
    network = Network(program)
    return network.run()

# Example usage
program = [int(x) for x in input().split(',')]
result = solve(program)
print(result)
