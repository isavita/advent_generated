class VM:
    def __init__(self, filename):
        self.code = {}
        self.ip = 0
        self.input = []
        self.output = []
        self.relative_base = 0
        self.load(filename)

    def load(self, filename):
        with open(filename, 'r') as f:
            data = [int(x) for x in f.read().strip().split(',')]
        for i, x in enumerate(data):
            self.code[i] = x

    def run(self):
        while True:
            cmd = Cmd(self.code.get(self.ip, 0))
            opcode = cmd.opcode()

            if opcode == 1:  # add
                arity = 3
                params = self.get_params_addresses(self.ip, cmd, arity)
                self.code[params[2]] = self.code.get(params[0], 0) + self.code.get(params[1], 0)
            elif opcode == 2:  # multiply
                arity = 3
                params = self.get_params_addresses(self.ip, cmd, arity)
                self.code[params[2]] = self.code.get(params[0], 0) * self.code.get(params[1], 0)
            elif opcode == 3:  # read
                arity = 1
                params = self.get_params_addresses(self.ip, cmd, arity)
                self.code[params[0]] = self.input.pop(0)
            elif opcode == 4:  # write
                arity = 1
                params = self.get_params_addresses(self.ip, cmd, arity)
                self.output.append(self.code.get(params[0], 0))
            elif opcode == 5:  # jump not zero
                arity = 2
                params = self.get_params_addresses(self.ip, cmd, arity)
                if self.code.get(params[0], 0) != 0:
                    self.ip = self.code.get(params[1], 0)
                    continue
            elif opcode == 6:  # jump zero
                arity = 2
                params = self.get_params_addresses(self.ip, cmd, arity)
                if self.code.get(params[0], 0) == 0:
                    self.ip = self.code.get(params[1], 0)
                    continue
            elif opcode == 7:  # less than
                arity = 3
                params = self.get_params_addresses(self.ip, cmd, arity)
                if self.code.get(params[0], 0) < self.code.get(params[1], 0):
                    self.code[params[2]] = 1
                else:
                    self.code[params[2]] = 0
            elif opcode == 8:  # equal
                arity = 3
                params = self.get_params_addresses(self.ip, cmd, arity)
                if self.code.get(params[0], 0) == self.code.get(params[1], 0):
                    self.code[params[2]] = 1
                else:
                    self.code[params[2]] = 0
            elif opcode == 9:  # change relative base
                arity = 1
                params = self.get_params_addresses(self.ip, cmd, arity)
                self.relative_base += self.code.get(params[0], 0)
            elif opcode == 99:  # halt
                return
            else:
                raise ValueError(f"Not an opcode: {cmd}")

            self.ip += arity + 1

    def get_params_addresses(self, pos, cmd, arity):
        modes = cmd.modes(arity)
        results = []
        for i in range(arity):
            results.append(self.get_param_address(pos + i + 1, modes[i]))
        return results

    def get_param_address(self, pos, mode):
        if mode == 0:  # position
            return self.code.get(pos, 0)
        elif mode == 1:  # immediate
            return pos
        elif mode == 2:  # relative
            return self.relative_base + self.code.get(pos, 0)
        else:
            raise ValueError(f"Wrong mode: {mode}")

class Cmd:
    def __init__(self, value):
        self.value = value

    def opcode(self):
        return self.value % 100

    def modes(self, arity):
        mode_section = self.value // 100
        modes = []
        for _ in range(arity):
            modes.append(mode_section % 10)
            mode_section //= 10
        return modes

def beam(x, y):
    vm = VM("input.txt")
    vm.input = [x, y]
    vm.run()
    return vm.output[-1] == 1

def main():
    y = 20
    x = 0

    while True:
        if not beam(x, y):
            x += 1
            continue

        if not beam(x + 99, y):
            y += 1
            continue

        if not beam(x, y + 99):
            x += 1
            continue

        print(x * 10000 + y)
        return

if __name__ == "__main__":
    main()