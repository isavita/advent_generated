import sys

def decode(n):
    op = n % 100
    modes = [(n // 100) % 10, (n // 1000) % 10, (n // 10000) % 10]
    return op, modes

class Machine:
    def __init__(self, program, in_stream, out_stream):
        self.data = {i: program[i] for i in range(len(program))}
        self.ip = 0
        self.in_stream = in_stream
        self.out_stream = out_stream
        self.relbase = 0

    def get(self, i, mo):
        if mo == 0:
            return self.data.get(self.data.get(i, 0), 0)
        elif mo == 1:
            return self.data.get(i, 0)
        elif mo == 2:
            return self.data.get(self.relbase + self.data.get(i, 0), 0)

    def set(self, i, mo, val):
        if mo == 0:
            self.data[self.data[i]] = val
        elif mo == 2:
            self.data[self.relbase + self.data[i]] = val

    def step(self):
        op, modes = decode(self.data.get(self.ip, 0))
        if op == 1:  # add
            val = self.get(self.ip+1, modes[0]) + self.get(self.ip+2, modes[1])
            self.set(self.ip+3, modes[2], val)
            self.ip += 4
        elif op == 2:  # mul
            val = self.get(self.ip+1, modes[0]) * self.get(self.ip+2, modes[1])
            self.set(self.ip+3, modes[2], val)
            self.ip += 4
        elif op == 3:  # input
            self.set(self.ip+1, modes[0], next(self.in_stream))
            self.ip += 2
        elif op == 4:  # output
            self.out_stream.append(self.get(self.ip+1, modes[0]))
            self.ip += 2
        elif op == 5:  # jt
            if self.get(self.ip+1, modes[0]) != 0:
                self.ip = self.get(self.ip+2, modes[1])
            else:
                self.ip += 3
        elif op == 6:  # jf
            if self.get(self.ip+1, modes[0]) == 0:
                self.ip = self.get(self.ip+2, modes[1])
            else:
                self.ip += 3
        elif op == 7:  # lt
            self.set(self.ip+3, modes[2], int(self.get(self.ip+1, modes[0]) < self.get(self.ip+2, modes[1])))
            self.ip += 4
        elif op == 8:  # eq
            self.set(self.ip+3, modes[2], int(self.get(self.ip+1, modes[0]) == self.get(self.ip+2, modes[1])))
            self.ip += 4
        elif op == 9:  # rbo
            self.relbase += self.get(self.ip+1, modes[0])
            self.ip += 2
        elif op == 99:  # halt
            return False
        return True

    def run(self):
        while self.step():
            pass

def run(program, in_stream):
    out_stream = []
    machine = Machine(program, in_stream, out_stream)
    machine.run()
    return out_stream

def parse(program):
    out = run(program, iter([]))
    scaffolding = {}
    robot = None
    dir = None
    x, y = 0, 0
    for o in out:
        c = chr(o)
        if c == '\n':
            y += 1
            x = 0
        else:
            if c in '^v<>':
                robot = (x, y)
                dir = {'^': 0, '>': 1, 'v': 2, '<': 3}[c]
                scaffolding[(x, y)] = '#'
            elif c == '#':
                scaffolding[(x, y)] = '#'
            x += 1
    return scaffolding, robot, dir

def sum_align(grid):
    sum_ = 0
    for (x, y) in grid:
        if all((x+dx, y+dy) in grid for dx, dy in [(0,1), (0,-1), (1,0), (-1,0)]):
            sum_ += x * y
    return sum_

def main():
    with open("input.txt", "r") as file:
        program = list(map(int, file.read().split(',')))
    scaffolding, robot, dir = parse(program)
    print(sum_align(scaffolding))

if __name__ == "__main__":
    main()