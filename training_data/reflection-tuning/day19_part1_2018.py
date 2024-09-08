class CPU:
    def __init__(self, ip_register, instructions):
        self.registers = [0] * 6
        self.ip = 0
        self.ip_register = ip_register
        self.instructions = instructions

    def addr(self, a, b, c):
        self.registers[c] = self.registers[a] + self.registers[b]

    def addi(self, a, b, c):
        self.registers[c] = self.registers[a] + b

    def mulr(self, a, b, c):
        self.registers[c] = self.registers[a] * self.registers[b]

    def muli(self, a, b, c):
        self.registers[c] = self.registers[a] * b

    def banr(self, a, b, c):
        self.registers[c] = self.registers[a] & self.registers[b]

    def bani(self, a, b, c):
        self.registers[c] = self.registers[a] & b

    def borr(self, a, b, c):
        self.registers[c] = self.registers[a] | self.registers[b]

    def bori(self, a, b, c):
        self.registers[c] = self.registers[a] | b

    def setr(self, a, b, c):
        self.registers[c] = self.registers[a]

    def seti(self, a, b, c):
        self.registers[c] = a

    def gtir(self, a, b, c):
        self.registers[c] = 1 if a > self.registers[b] else 0

    def gtri(self, a, b, c):
        self.registers[c] = 1 if self.registers[a] > b else 0

    def gtrr(self, a, b, c):
        self.registers[c] = 1 if self.registers[a] > self.registers[b] else 0

    def eqir(self, a, b, c):
        self.registers[c] = 1 if a == self.registers[b] else 0

    def eqri(self, a, b, c):
        self.registers[c] = 1 if self.registers[a] == b else 0

    def eqrr(self, a, b, c):
        self.registers[c] = 1 if self.registers[a] == self.registers[b] else 0

    def execute(self):
        while 0 <= self.ip < len(self.instructions):
            self.registers[self.ip_register] = self.ip
            opcode, a, b, c = self.instructions[self.ip]
            getattr(self, opcode)(a, b, c)
            self.ip = self.registers[self.ip_register]
            self.ip += 1
        return self.registers[0]

def parse_input(filename):
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    ip_register = int(lines[0].split()[1])
    instructions = []
    for line in lines[1:]:
        opcode, *params = line.split()
        instructions.append((opcode, *map(int, params)))
    
    return ip_register, instructions

def main():
    ip_register, instructions = parse_input('input.txt')
    cpu = CPU(ip_register, instructions)
    result = cpu.execute()
    print(result)

if __name__ == "__main__":
    main()
