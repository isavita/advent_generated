
import itertools
import math
import queue
import threading

class Cmd:
    def __init__(self, value):
        self.value = value

    def op_code(self):
        return self.value % 100

    def immediate(self, param_num):
        digit = 10 ** (param_num + 1)
        return (self.value // digit) % 10 == 1

class VM:
    def __init__(self, code, input_queue, output_queue):
        self.code = code
        self.ip = 0
        self.input_queue = input_queue
        self.output_queue = output_queue

    def load(self, filename):
        with open(filename) as f:
            self.code = [int(x) for x in f.read().strip().split(',')]

    def run(self):
        while True:
            cmd = Cmd(self.code[self.ip])

            if cmd.op_code() == 1:
                param1 = self.get_param(self.ip + 1, cmd.immediate(1))
                param2 = self.get_param(self.ip + 2, cmd.immediate(2))
                address = self.get_param(self.ip + 3, True)
                self.code[address] = param1 + param2
                self.ip += 4

            elif cmd.op_code() == 2:
                param1 = self.get_param(self.ip + 1, cmd.immediate(1))
                param2 = self.get_param(self.ip + 2, cmd.immediate(2))
                address = self.get_param(self.ip + 3, True)
                self.code[address] = param1 * param2
                self.ip += 4

            elif cmd.op_code() == 3:
                address = self.get_param(self.ip + 1, True)
                self.code[address] = self.input_queue.get()
                self.ip += 2

            elif cmd.op_code() == 4:
                param1 = self.get_param(self.ip + 1, cmd.immediate(1))
                self.output_queue.put(param1)
                self.ip += 2

            elif cmd.op_code() == 5:
                param1 = self.get_param(self.ip + 1, cmd.immediate(1))
                param2 = self.get_param(self.ip + 2, cmd.immediate(2))
                if param1 != 0:
                    self.ip = param2
                else:
                    self.ip += 3

            elif cmd.op_code() == 6:
                param1 = self.get_param(self.ip + 1, cmd.immediate(1))
                param2 = self.get_param(self.ip + 2, cmd.immediate(2))
                if param1 == 0:
                    self.ip = param2
                else:
                    self.ip += 3

            elif cmd.op_code() == 7:
                param1 = self.get_param(self.ip + 1, cmd.immediate(1))
                param2 = self.get_param(self.ip + 2, cmd.immediate(2))
                address = self.get_param(self.ip + 3, True)
                if param1 < param2:
                    self.code[address] = 1
                else:
                    self.code[address] = 0
                self.ip += 4

            elif cmd.op_code() == 8:
                param1 = self.get_param(self.ip + 1, cmd.immediate(1))
                param2 = self.get_param(self.ip + 2, cmd.immediate(2))
                address = self.get_param(self.ip + 3, True)
                if param1 == param2:
                    self.code[address] = 1
                else:
                    self.code[address] = 0
                self.ip += 4

            elif cmd.op_code() == 99:
                return

            else:
                raise ValueError(f"Not an opcode {cmd.op_code()}")

    def get_param(self, address, immediate):
        param = self.code[address]
        if immediate:
            return param
        return self.code[param]

def run_loop(phase):
    chs = [queue.Queue(1) for _ in range(6)]
    threads = []

    for i in range(5):
        chs[i].put(phase[i])
        vm = VM([], chs[i], chs[(i + 1) % 5])
        vm.load("input.txt")
        thread = threading.Thread(target=vm.run)
        threads.append(thread)
        thread.start()

    chs[0].put(0)

    for thread in threads:
        thread.join()

    return chs[0].get()

def main():
    max_output = 0
    for phase in itertools.permutations([5, 6, 7, 8, 9]):
        output = run_loop(list(phase))
        if output > max_output:
            max_output = output

    print(max_output)

if __name__ == "__main__":
    main()
