
import sys
from collections import defaultdict
from typing import List

class Mode:
    POSITION = 0
    IMMEDIATE = 1
    RELATIVE = 2

class Opcode:
    ADD = 1
    MUL = 2
    INPUT = 3
    OUTPUT = 4
    JT = 5
    JF = 6
    LT = 7
    EQ = 8
    RBO = 9
    HALT = 99

def decode(n):
    op = n % 100
    n //= 100
    modes = [n % 10, (n // 10) % 10, n // 100]
    return op, modes

class Machine:
    def __init__(self, program: List[int], in_stream, out_stream):
        self.data = defaultdict(int, enumerate(program))
        self.ip = 0
        self.in_stream = in_stream
        self.out_stream = out_stream
        self.rel_base = 0

    def get(self, i, mode):
        if mode == Mode.IMMEDIATE:
            return self.data[i]
        if mode == Mode.POSITION:
            return self.data[self.data[i]]
        if mode == Mode.RELATIVE:
            return self.data[self.rel_base + self.data[i]]
        raise ValueError(f"Unknown mode: {mode}")

    def set(self, i, mode, val):
        if mode == Mode.POSITION:
            self.data[self.data[i]] = val
        elif mode == Mode.RELATIVE:
            self.data[self.rel_base + self.data[i]] = val
        else:
            raise ValueError(f"Unknown mode: {mode}")

    def step(self):
        op, modes = decode(self.data[self.ip])
        if op == Opcode.ADD:
            val = self.get(self.ip + 1, modes[0]) + self.get(self.ip + 2, modes[1])
            self.set(self.ip + 3, modes[2], val)
            self.ip += 4
        elif op == Opcode.MUL:
            val = self.get(self.ip + 1, modes[0]) * self.get(self.ip + 2, modes[1])
            self.set(self.ip + 3, modes[2], val)
            self.ip += 4
        elif op == Opcode.INPUT:
            self.set(self.ip + 1, modes[0], next(self.in_stream))
            self.ip += 2
        elif op == Opcode.OUTPUT:
            self.out_stream.append(self.get(self.ip + 1, modes[0]))
            self.ip += 2
        elif op == Opcode.JT:
            if self.get(self.ip + 1, modes[0]) != 0:
                self.ip = self.get(self.ip + 2, modes[1])
            else:
                self.ip += 3
        elif op == Opcode.JF:
            if self.get(self.ip + 1, modes[0]) == 0:
                self.ip = self.get(self.ip + 2, modes[1])
            else:
                self.ip += 3
        elif op == Opcode.LT:
            if self.get(self.ip + 1, modes[0]) < self.get(self.ip + 2, modes[1]):
                self.set(self.ip + 3, modes[2], 1)
            else:
                self.set(self.ip + 3, modes[2], 0)
            self.ip += 4
        elif op == Opcode.EQ:
            if self.get(self.ip + 1, modes[0]) == self.get(self.ip + 2, modes[1]):
                self.set(self.ip + 3, modes[2], 1)
            else:
                self.set(self.ip + 3, modes[2], 0)
            self.ip += 4
        elif op == Opcode.RBO:
            self.rel_base += self.get(self.ip + 1, modes[0])
            self.ip += 2
        elif op == Opcode.HALT:
            return False
        else:
            raise ValueError(f"Unknown opcode: {op}")
        return True

    def run(self):
        while self.step():
            pass

def run(program):
    out_stream = []
    m = Machine(program, iter([]), out_stream)
    m.run()
    return out_stream

def count_blocks(program):
    grid = {}
    out_stream = run(program)
    while out_stream:
        x, y, t = out_stream.pop(0), out_stream.pop(0), out_stream.pop(0)
        grid[(x, y)] = t
    return list(grid.values()).count(2)

def read_all(filepath):
    with open(filepath, 'r') as file:
        return file.read().strip()

def to_int(s):
    return int(s)

def main():
    filepath = "input.txt"
    program = [to_int(n) for n in read_all(filepath).split(",")]
    print(count_blocks(program))

if __name__ == "__main__":
    main()
