
#!/bin/bash

# Save the Python code to a temporary file
cat > /tmp/intcode_network.py << 'EOF'
import sys
from collections import deque, defaultdict

# Read the Intcode program from 'input.txt'
try:
    with open('input.txt') as f:
        program = [int(x) for x in f.read().strip().split(',')]
except FileNotFoundError:
    print("Error: input.txt not found.", file=sys.stderr)
    sys.exit(1)
except Exception as e:
    print(f"Error reading input.txt: {e}", file=sys.stderr)
    sys.exit(1)

class IntcodeComputer:
    def __init__(self, program, inputs=None):
        self.memory = defaultdict(int)
        for i, val in enumerate(program):
            self.memory[i] = val
        self.ip = 0
        self.relative_base = 0
        self.inputs = deque(inputs or [])
        self.outputs = []
        self.halted = False
        self.needs_input = False

    def get_param(self, mode, offset):
        addr = self.ip + offset
        val_at_addr = self.memory[addr]
        if mode == 0:
            return self.memory[val_at_addr]
        elif mode == 1:
            return val_at_addr
        elif mode == 2:
            return self.memory[self.relative_base + val_at_addr]
        else:
            raise Exception(f"Unknown mode {mode}")

    def set_param(self, mode, offset, value):
        addr = self.ip + offset
        val_at_addr = self.memory[addr]
        if mode == 0:
            self.memory[val_at_addr] = value
        elif mode == 2:
            self.memory[self.relative_base + val_at_addr] = value
        else:
            raise Exception(f"Unknown mode {mode} for setting parameter")

    def run(self):
        self.needs_input = False
        while not self.halted:
            instruction = self.memory[self.ip]
            opcode = instruction % 100
            modes = [
                (instruction // 100) % 10,
                (instruction // 1000) % 10,
                (instruction // 10000) % 10
            ]

            if opcode == 99:
                self.halted = True
                break
            elif opcode == 1: # add
                p1 = self.get_param(modes[0], 1)
                p2 = self.get_param(modes[1], 2)
                self.set_param(modes[2], 3, p1 + p2)
                self.ip += 4
            elif opcode == 2: # multiply
                p1 = self.get_param(modes[0], 1)
                p2 = self.get_param(modes[1], 2)
                self.set_param(modes[2], 3, p1 * p2)
                self.ip += 4
            elif opcode == 3: # input
                if not self.inputs:
                    self.needs_input = True
                    return
                value = self.inputs.popleft()
                self.set_param(modes[0], 1, value)
                self.ip += 2
            elif opcode == 4: # output
                p1 = self.get_param(modes[0], 1)
                self.outputs.append(p1)
                self.ip += 2
                if len(self.outputs) == 3:
                     return # Return after producing a full packet
            elif opcode == 5: # jump-if-true
                p1 = self.get_param(modes[0], 1)
                p2 = self.get_param(modes[1], 2)
                if p1 != 0:
                    self.ip = p2
                else:
                    self.ip += 3
            elif opcode == 6: # jump-if-false
                p1 = self.get_param(modes[0], 1)
                p2 = self.get_param(modes[1], 2)
                if p1 == 0:
                    self.ip = p2
                else:
                    self.ip += 3
            elif opcode == 7: # less than
                p1 = self.get_param(modes[0], 1)
                p2 = self.get_param(modes[1], 2)
                self.set_param(modes[2], 3, 1 if p1 < p2 else 0)
                self.ip += 4
            elif opcode == 8: # equals
                p1 = self.get_param(modes[0], 1)
                p2 = self.get_param(modes[1], 2)
                self.set_param(modes[2], 3, 1 if p1 == p2 else 0)
                self.ip += 4
            elif opcode == 9: # adjust relative base
                p1 = self.get_param(modes[0], 1)
                self.relative_base += p1
                self.ip += 2
            else:
                raise Exception(f"Unknown opcode {opcode} at ip {self.ip}")

computers = []
for address in range(50):
    computer = IntcodeComputer(program.copy(), inputs=[address])
    computers.append(computer)

packet_queues = [deque() for _ in range(50)]

first_packet_to_255_y = None

while True:
    idle = True
    for i in range(50):
        computer = computers[i]

        if not computer.inputs:
             if packet_queues[i]:
                 x, y = packet_queues[i].popleft()
                 computer.inputs.extend([x, y])
                 idle = False
             else:
                 computer.inputs.append(-1)
        else:
             # If inputs were already present (e.g. from previous partial execution)
             # or just got added from queue, don't immediately mark as idle
             if any(inp != -1 for inp in computer.inputs):
                 idle = False


        computer.run()

        while len(computer.outputs) >= 3:
            idle = False
            dest = computer.outputs.pop(0)
            x = computer.outputs.pop(0)
            y = computer.outputs.pop(0)

            if dest == 255:
                if first_packet_to_255_y is None:
                    first_packet_to_255_y = y
                    print(first_packet_to_255_y)
                    sys.exit(0) # Found the first packet, exit successfully
            elif 0 <= dest < 50:
                packet_queues[dest].append((x, y))
            # else: ignore packets to other addresses
EOF

main() {
    if [[ ! -f "input.txt" ]]; then
       echo "Error: input.txt not found." >&2
       exit 1
    fi
    python3 /tmp/intcode_network.py
    rm /tmp/intcode_network.py # Clean up the temporary file
}

main
