import sys
from collections import deque, defaultdict

# Read the Intcode program from 'input.txt'
with open('input.txt') as f:
    program = [int(x) for x in f.read().strip().split(',')]

class IntcodeComputer:
    def __init__(self, program, inputs=None):
        self.memory = defaultdict(int, enumerate(program))
        self.ip = 0  # Instruction pointer
        self.relative_base = 0
        self.inputs = deque(inputs or [])
        self.outputs = []
        self.halted = False
        self.idle = False  # To track if the computer is idle (waiting for input and has no output)

    def get_param(self, mode, offset):
        if mode == 0:  # Position mode
            return self.memory[self.memory[self.ip + offset]]
        elif mode == 1:  # Immediate mode
            return self.memory[self.ip + offset]
        elif mode == 2:  # Relative mode
            return self.memory[self.relative_base + self.memory[self.ip + offset]]
        else:
            raise Exception(f"Unknown mode {mode}")

    def set_param(self, mode, offset, value):
        if mode == 0:
            self.memory[self.memory[self.ip + offset]] = value
        elif mode == 2:
            self.memory[self.relative_base + self.memory[self.ip + offset]] = value
        else:
            raise Exception(f"Unknown mode {mode}")

    def run(self):
        while True:
            opcode = self.memory[self.ip] % 100
            modes = [
                (self.memory[self.ip] // 100) % 10,
                (self.memory[self.ip] // 1000) % 10,
                (self.memory[self.ip] // 10000) % 10
            ]

            if opcode == 99:
                self.halted = True
                break
            elif opcode in (1, 2, 7, 8):
                param1 = self.get_param(modes[0], 1)
                param2 = self.get_param(modes[1], 2)
                if opcode == 1:
                    result = param1 + param2
                elif opcode == 2:
                    result = param1 * param2
                elif opcode == 7:
                    result = int(param1 < param2)
                elif opcode == 8:
                    result = int(param1 == param2)
                self.set_param(modes[2], 3, result)
                self.ip += 4
            elif opcode == 3:
                if not self.inputs:
                    self.set_param(modes[0], 1, -1)
                    self.ip += 2
                    self.idle = True  # No input available, computer is idle
                    return  # Wait for input
                else:
                    value = self.inputs.popleft()
                    self.set_param(modes[0], 1, value)
                    self.ip += 2
                    self.idle = False
            elif opcode == 4:
                param1 = self.get_param(modes[0], 1)
                self.outputs.append(param1)
                self.ip += 2
                self.idle = False  # Output produced, computer is active
                if len(self.outputs) == 3:
                    return  # Output is ready
            elif opcode in (5, 6):
                param1 = self.get_param(modes[0], 1)
                param2 = self.get_param(modes[1], 2)
                if (opcode == 5 and param1 != 0) or (opcode == 6 and param1 == 0):
                    self.ip = param2
                else:
                    self.ip += 3
            elif opcode == 9:
                param1 = self.get_param(modes[0], 1)
                self.relative_base += param1
                self.ip += 2
            else:
                raise Exception(f"Unknown opcode {opcode}")

# Initialize 50 computers
computers = []
for address in range(50):
    computer = IntcodeComputer(program.copy(), inputs=[address])
    computers.append(computer)

# Packet queues for each computer
packet_queues = [deque() for _ in range(50)]

# Start the network
nat_packet = None
prev_nat_y = None

while True:
    network_idle = True  # Assume network is idle unless any computer does something
    for i, computer in enumerate(computers):
        if packet_queues[i]:
            # Provide X and Y of the next packet
            x, y = packet_queues[i].popleft()
            computer.inputs.extend([x, y])
            computer.idle = False  # Computer has input, so it's not idle
        else:
            # Provide -1 if no packet is waiting
            computer.inputs.append(-1)
            # The computer's idle status will be set in its run method

        computer.run()

        # Process outputs
        while len(computer.outputs) >= 3:
            network_idle = False  # Since this computer sent a packet, network is not idle
            dest, x, y = computer.outputs[:3]
            del computer.outputs[:3]

            dest = int(dest)
            x = int(x)
            y = int(y)

            if dest == 255:
                # NAT handles packets sent to address 255
                nat_packet = (x, y)
            elif 0 <= dest < 50:
                packet_queues[dest].append((x, y))
            else:
                # Ignore invalid addresses
                pass

    # Check if network is idle
    if all(not packet_queues[i] and computers[i].idle for i in range(50)):
        # Network is idle
        if nat_packet:
            x, y = nat_packet
            packet_queues[0].append((x, y))
            if y == prev_nat_y:
                print(y)
                sys.exit(0)
            prev_nat_y = y
        network_idle = False  # Since we sent a packet, network is no longer idle