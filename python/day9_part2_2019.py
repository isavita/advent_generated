
def run_intcode(memory, input_val):
    output = 0
    ip = 0
    relative_base = 0

    while True:
        opcode = memory[ip] % 100
        modes = str(memory[ip] // 100)

        def get_param(offset):
            mode = 0
            if len(modes) >= offset:
                mode = int(modes[-offset])
            
            param = memory[ip + offset]
            if mode == 0:
                return memory[param]
            elif mode == 1:
                return param
            elif mode == 2:
                return memory[relative_base + param]
            else:
                raise Exception("Unknown parameter mode")

        def set_param(offset, value):
            mode = 0
            if len(modes) >= offset:
                mode = int(modes[-offset])
            
            param = memory[ip + offset]
            if mode == 0:
                memory[param] = value
            elif mode == 2:
                memory[relative_base + param] = value
            else:
                raise Exception("Unknown parameter mode")

        if opcode == 1:
            set_param(3, get_param(1) + get_param(2))
            ip += 4
        elif opcode == 2:
            set_param(3, get_param(1) * get_param(2))
            ip += 4
        elif opcode == 3:
            set_param(1, input_val)
            ip += 2
        elif opcode == 4:
            output = get_param(1)
            ip += 2
        elif opcode == 5:
            if get_param(1) != 0:
                ip = get_param(2)
            else:
                ip += 3
        elif opcode == 6:
            if get_param(1) == 0:
                ip = get_param(2)
            else:
                ip += 3
        elif opcode == 7:
            if get_param(1) < get_param(2):
                set_param(3, 1)
            else:
                set_param(3, 0)
            ip += 4
        elif opcode == 8:
            if get_param(1) == get_param(2):
                set_param(3, 1)
            else:
                set_param(3, 0)
            ip += 4
        elif opcode == 9:
            relative_base += get_param(1)
            ip += 2
        elif opcode == 99:
            return output
        else:
            raise Exception("Unknown opcode: {}".format(opcode))

with open("input.txt", "r") as file:
    program = list(map(int, file.read().strip().split(",")))

memory = {i: program[i] for i in range(len(program))}
result = run_intcode(memory, 2)
print(result)
