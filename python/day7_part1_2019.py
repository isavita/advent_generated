import math
import threading
from itertools import permutations
from queue import Queue

class VM:
    def __init__(self, filename, input_queue, output_queue):
        self.code = []
        self.ip = 0
        self.input_queue = input_queue
        self.output_queue = output_queue
        self.load(filename)

    def load(self, filename):
        with open(filename, 'r') as file:
            self.code = [int(x) for x in file.read().strip().split(',')]

    def run(self):
        while True:
            cmd = self.code[self.ip]
            opcode = cmd % 100

            if opcode == 1:
                # add
                param1 = self.get_param(self.ip + 1, self.is_immediate(cmd, 1))
                param2 = self.get_param(self.ip + 2, self.is_immediate(cmd, 2))
                address = self.get_param(self.ip + 3, True)
                self.code[address] = param1 + param2
                self.ip += 4
            elif opcode == 2:
                # multiply
                param1 = self.get_param(self.ip + 1, self.is_immediate(cmd, 1))
                param2 = self.get_param(self.ip + 2, self.is_immediate(cmd, 2))
                address = self.get_param(self.ip + 3, True)
                self.code[address] = param1 * param2
                self.ip += 4
            elif opcode == 3:
                # read
                address = self.get_param(self.ip + 1, True)
                self.code[address] = self.input_queue.get()
                self.ip += 2
            elif opcode == 4:
                # write
                param1 = self.get_param(self.ip + 1, self.is_immediate(cmd, 1))
                self.output_queue.put(param1)
                self.ip += 2
            elif opcode == 5:
                # jump not zero
                param1 = self.get_param(self.ip + 1, self.is_immediate(cmd, 1))
                param2 = self.get_param(self.ip + 2, self.is_immediate(cmd, 2))
                if param1 != 0:
                    self.ip = param2
                else:
                    self.ip += 3
            elif opcode == 6:
                # jump zero
                param1 = self.get_param(self.ip + 1, self.is_immediate(cmd, 1))
                param2 = self.get_param(self.ip + 2, self.is_immediate(cmd, 2))
                if param1 == 0:
                    self.ip = param2
                else:
                    self.ip += 3
            elif opcode == 7:
                # less than
                param1 = self.get_param(self.ip + 1, self.is_immediate(cmd, 1))
                param2 = self.get_param(self.ip + 2, self.is_immediate(cmd, 2))
                address = self.get_param(self.ip + 3, True)
                self.code[address] = 1 if param1 < param2 else 0
                self.ip += 4
            elif opcode == 8:
                # equal
                param1 = self.get_param(self.ip + 1, self.is_immediate(cmd, 1))
                param2 = self.get_param(self.ip + 2, self.is_immediate(cmd, 2))
                address = self.get_param(self.ip + 3, True)
                self.code[address] = 1 if param1 == param2 else 0
                self.ip += 4
            elif opcode == 99:
                # halt
                return
            else:
                raise ValueError(f"Invalid opcode: {opcode}")

    def get_param(self, address, immediate):
        return self.code[address] if immediate else self.code[self.code[address]]

    def is_immediate(self, cmd, param_num):
        return (cmd // 10 ** (param_num + 1)) % 10 == 1

def run_amplifiers(phase):
    queues = [Queue() for _ in range(6)]

    for i in range(5):
        queues[i].put(phase[i])
        threading.Thread(target=VM("input.txt", queues[i], queues[i + 1]).run).start()

    queues[0].put(0)
    return queues[5].get()

def main():
    max_output = 0
    for phase in permutations(range(5)):
        output = run_amplifiers(phase)
        max_output = max(max_output, output)
    print(max_output)

if __name__ == "__main__":
    main()
