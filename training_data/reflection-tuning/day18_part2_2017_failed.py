from collections import defaultdict, deque

class Program:
    def __init__(self, instructions, pid):
        self.instructions = instructions
        self.registers = defaultdict(int)
        self.registers['p'] = pid
        self.pc = 0
        self.queue = deque()
        self.waiting = False
        self.send_count = 0

    def run(self, other_program):
        while 0 <= self.pc < len(self.instructions):
            inst = self.instructions[self.pc].split()
            op = inst[0]
            x = inst[1]
            y = inst[2] if len(inst) > 2 else None

            if op == 'snd':
                value = self.get_value(x)
                other_program.queue.append(value)
                self.send_count += 1
            elif op == 'set':
                self.registers[x] = self.get_value(y)
            elif op == 'add':
                self.registers[x] += self.get_value(y)
            elif op == 'mul':
                self.registers[x] *= self.get_value(y)
            elif op == 'mod':
                self.registers[x] %= self.get_value(y)
            elif op == 'rcv':
                if self.queue:
                    self.registers[x] = self.queue.popleft()
                    self.waiting = False
                else:
                    self.waiting = True
                    return
            elif op == 'jgz':
                if self.get_value(x) > 0:
                    self.pc += self.get_value(y) - 1

            self.pc += 1

    def get_value(self, x):
        return self.registers[x] if x.isalpha() else int(x)

def solve(instructions):
    p0 = Program(instructions, 0)
    p1 = Program(instructions, 1)

    while True:
        p0.run(p1)
        p1.run(p0)
        if p0.waiting and p1.waiting and not p0.queue and not p1.queue:
            break

    return p1.send_count

# Example usage
instructions = [
    "snd 1",
    "snd 2",
    "snd p",
    "rcv a",
    "rcv b",
    "rcv c",
    "rcv d"
]

result = solve(instructions)
print(f"Program 1 sent a value {result} times.")
