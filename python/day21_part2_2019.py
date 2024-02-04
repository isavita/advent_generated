
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
        with open(filename, 'r') as file:
            list_str = file.read().strip().split(',')
        for i, val in enumerate(list_str):
            self.code[i] = int(val)
    
    def run(self):
        while True:
            cmd = self.code.get(self.ip, 0)
            opcode = cmd % 100
            modes = [(cmd // 10 ** (2 + i)) % 10 for i in range(3)]
            
            def get_param(index):
                mode = modes[index - 1]
                value = self.code.get(self.ip + index, 0)
                if mode == 0:  # Position mode
                    return self.code.get(value, 0)
                elif mode == 1:  # Immediate mode
                    return value
                elif mode == 2:  # Relative mode
                    return self.code.get(self.relative_base + value, 0)
            
            def get_address(index):
                mode = modes[index - 1]
                value = self.code.get(self.ip + index, 0)
                if mode == 2:
                    return self.relative_base + value
                return value
            
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
                self.code[get_address(3)] = 1 if get_param(1) < get_param(2) else 0
                self.ip += 4
            elif opcode == 8:  # equals
                self.code[get_address(3)] = 1 if get_param(1) == get_param(2) else 0
                self.ip += 4
            elif opcode == 9:  # adjust relative base
                self.relative_base += get_param(1)
                self.ip += 2
            elif opcode == 99:  # halt
                break
            else:
                raise Exception(f"Unknown opcode {opcode}")
    
    def send_string(self, s):
        for c in s:
            self.input.append(ord(c))
        self.input.append(10)  # newline character
    
def main():
    vm = VM("input.txt")
    instructions = [
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "NOT A T",
        "AND A T",
        "OR E T",
        "OR H T",
        "AND T J",
        "RUN",
    ]
    for i in instructions:
        vm.send_string(i)
    vm.run()
    for output in vm.output:
        print(output)

if __name__ == "__main__":
    main()
