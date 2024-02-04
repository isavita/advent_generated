
import math

class VM:
    def __init__(self, filename):
        self.code = {}
        self.ip = 0
        self.input = []
        self.output = []
        self.relative_base = 0
        self.load(filename)
    
    def load(self, filename):
        with open(filename) as f:
            list_str = f.read().strip().split(',')
        for i, item in enumerate(list_str):
            self.code[i] = int(item)
    
    def run(self):
        while True:
            cmd = self.code.get(self.ip, 0)
            opcode = cmd % 100
            modes = [(cmd // 10 ** i) % 10 for i in range(2, 5)]
            
            def get_param(index):
                mode = modes[index - 1]
                val = self.code.get(self.ip + index, 0)
                if mode == 0:  # Position mode
                    return self.code.get(val, 0)
                elif mode == 1:  # Immediate mode
                    return val
                elif mode == 2:  # Relative mode
                    return self.code.get(self.relative_base + val, 0)
            
            def get_address(index):
                mode = modes[index - 1]
                val = self.code.get(self.ip + index, 0)
                if mode == 0:
                    return val
                elif mode == 2:
                    return self.relative_base + val
            
            if opcode == 1:  # add
                self.code[get_address(3)] = get_param(1) + get_param(2)
                self.ip += 4
            elif opcode == 2:  # multiply
                self.code[get_address(3)] = get_param(1) * get_param(2)
                self.ip += 4
            elif opcode == 3:  # input
                self.code[get_address(1)] = self.input.pop(0)
                self.ip += 2
            elif opcode == 4:  # output
                self.output.append(get_param(1))
                self.ip += 2
            elif opcode == 5:  # jump-if-true
                if get_param(1) != 0:
                    self.ip = get_param(2)
                else:
                    self.ip += 3
            elif opcode == 6:  # jump-if-false
                if get_param(1) == 0:
                    self.ip = get_param(2)
                else:
                    self.ip += 3
            elif opcode == 7:  # less than
                self.code[get_address(3)] = int(get_param(1) < get_param(2))
                self.ip += 4
            elif opcode == 8:  # equals
                self.code[get_address(3)] = int(get_param(1) == get_param(2))
                self.ip += 4
            elif opcode == 9:  # adjust relative base
                self.relative_base += get_param(1)
                self.ip += 2
            elif opcode == 99:  # halt
                break
            else:
                raise Exception(f"Unknown opcode {opcode}")

def beam(x, y, filename):
    vm = VM(filename)
    vm.input = [x, y]
    vm.run()
    return vm.output[0] == 1

def main():
    sum = 0
    for y in range(50):
        for x in range(50):
            if beam(x, y, "input.txt"):
                sum += 1
    print(sum)

if __name__ == "__main__":
    main()
