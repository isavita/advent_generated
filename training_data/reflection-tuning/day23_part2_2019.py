from collections import deque

class IntcodeComputer:
    def __init__(self, program, address):
        self.memory = program.copy()
        self.ip = 0
        self.relative_base = 0
        self.address = address
        self.input_queue = deque([address])
        self.output_buffer = []

    def run(self):
        while True:
            opcode = self.memory[self.ip] % 100
            modes = [int(d) for d in f"{self.memory[self.ip]:05d}"[:3]][::-1]

            if opcode == 99:
                return None

            if opcode == 3:
                if not self.input_queue:
                    return -1
                value = self.input_queue.popleft()
                self.write(1, value)
                self.ip += 2
            elif opcode == 4:
                value = self.read(1)
                self.output_buffer.append(value)
                self.ip += 2
                if len(self.output_buffer) == 3:
                    return self.output_buffer
            else:
                self.execute_instruction(opcode, modes)

    def read(self, offset):
        mode = self.memory[self.ip] // (10 * 10**offset) % 10
        value = self.memory[self.ip + offset]
        if mode == 0:
            return self.memory[value]
        elif mode == 1:
            return value
        elif mode == 2:
            return self.memory[self.relative_base + value]

    def write(self, offset, value):
        mode = self.memory[self.ip] // (10 * 10**offset) % 10
        address = self.memory[self.ip + offset]
        if mode == 2:
            address += self.relative_base
        self.memory[address] = value

    def execute_instruction(self, opcode, modes):
        if opcode in [1, 2, 7, 8]:
            a, b = self.read(1), self.read(2)
            result = {1: a + b, 2: a * b, 7: int(a < b), 8: int(a == b)}[opcode]
            self.write(3, result)
            self.ip += 4
        elif opcode in [5, 6]:
            a, b = self.read(1), self.read(2)
            if (opcode == 5 and a != 0) or (opcode == 6 and a == 0):
                self.ip = b
            else:
                self.ip += 3
        elif opcode == 9:
            self.relative_base += self.read(1)
            self.ip += 2

class NAT:
    def __init__(self):
        self.last_packet = None
        self.last_y_sent = None

    def receive_packet(self, x, y):
        self.last_packet = (x, y)

    def send_packet(self, computers):
        if self.last_packet:
            computers[0].input_queue.extend(self.last_packet)
            if self.last_y_sent == self.last_packet[1]:
                return self.last_packet[1]
            self.last_y_sent = self.last_packet[1]
        return None

def solve_network_problem(program):
    computers = {i: IntcodeComputer(program, i) for i in range(50)}
    nat = NAT()
    part1_solved = False

    while True:
        idle = True
        for i in range(50):
            result = computers[i].run()
            if result == -1:
                computers[i].input_queue.append(-1)
            elif result is not None:
                idle = False
                dest, x, y = result
                computers[i].output_buffer.clear()
                if dest == 255:
                    nat.receive_packet(x, y)
                    if not part1_solved:
                        print(f"Part 1: {y}")
                        part1_solved = True
                else:
                    computers[dest].input_queue.extend([x, y])

        if idle:
            result = nat.send_packet(computers)
            if result is not None:
                print(f"Part 2: {result}")
                break

if __name__ == "__main__":
    with open("input.txt", "r") as f:
        program = list(map(int, f.read().strip().split(",")))
    program += [0] * 10000  # Extend memory
    solve_network_problem(program)
