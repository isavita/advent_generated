with open("input.txt") as f:
    instructions = list(map(int, f.read().split(',')))

def get_value(instructions, index, mode):
    if mode == 0:
        return instructions[instructions[index]]
    else:
        return instructions[index]

def run_program(instructions, input_val):
    index = 0
    while index < len(instructions):
        opcode = instructions[index] % 100
        mode1 = instructions[index] // 100 % 10
        mode2 = instructions[index] // 1000 % 10
        
        if opcode == 99:
            break
        elif opcode == 1:
            val1 = get_value(instructions, index + 1, mode1)
            val2 = get_value(instructions, index + 2, mode2)
            instructions[instructions[index + 3]] = val1 + val2
            index += 4
        elif opcode == 2:
            val1 = get_value(instructions, index + 1, mode1)
            val2 = get_value(instructions, index + 2, mode2)
            instructions[instructions[index + 3]] = val1 * val2
            index += 4
        elif opcode == 3:
            instructions[instructions[index + 1]] = input_val
            index += 2
        elif opcode == 4:
            print(get_value(instructions, index + 1, mode1))
            index += 2
        elif opcode == 5:
            val1 = get_value(instructions, index + 1, mode1)
            val2 = get_value(instructions, index + 2, mode2)
            if val1 != 0:
                index = val2
            else:
                index += 3
        elif opcode == 6:
            val1 = get_value(instructions, index + 1, mode1)
            val2 = get_value(instructions, index + 2, mode2)
            if val1 == 0:
                index = val2
            else:
                index += 3
        elif opcode == 7:
            val1 = get_value(instructions, index + 1, mode1)
            val2 = get_value(instructions, index + 2, mode2)
            if val1 < val2:
                instructions[instructions[index + 3]] = 1
            else:
                instructions[instructions[index + 3]] = 0
            index += 4
        elif opcode == 8:
            val1 = get_value(instructions, index + 1, mode1)
            val2 = get_value(instructions, index + 2, mode2)
            if val1 == val2:
                instructions[instructions[index + 3]] = 1
            else:
                instructions[instructions[index + 3]] = 0
            index += 4

run_program(instructions, 5)