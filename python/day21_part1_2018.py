import re

def solve(input_data):
    opcode_computer = parse_input(input_data)

    while not opcode_computer.tick():
        if opcode_computer.registers[opcode_computer.instruction_pointer] == 28:
            break

    return opcode_computer.registers[5]

class OpcodeComputer:
    def __init__(self, instructions, instruction_pointer):
        self.instructions = instructions
        self.registers = [0] * 6
        self.instruction_pointer = instruction_pointer

    def tick(self):
        if self.registers[self.instruction_pointer] >= len(self.instructions):
            print("Out of range instruction, terminating...")
            return True

        inst_index = self.registers[self.instruction_pointer]
        inst = self.instructions[inst_index]

        opcode_func = opcode_names_to_funcs[inst.name]

        self.registers = opcode_func(self.registers, inst.abc_values)

        self.registers[self.instruction_pointer] += 1

        if self.registers[self.instruction_pointer] >= len(self.instructions):
            return True

        return False

class Instruction:
    def __init__(self, name, abc_values):
        self.name = name
        self.abc_values = abc_values

def parse_input(input_data):
    lines = input_data.split("\n")

    instruction_pointer = int(lines[0].split()[-1])

    instructions = []
    for line in lines[1:]:
        if not line.strip():
            continue
        parts = line.split()
        name = parts[0]
        abc_values = list(map(int, parts[1:]))
        instructions.append(Instruction(name, abc_values))

    return OpcodeComputer(instructions, instruction_pointer)

def addr(registers, abc_values):
    registers[abc_values[2]] = registers[abc_values[0]] + registers[abc_values[1]]
    return registers

def addi(registers, abc_values):
    registers[abc_values[2]] = registers[abc_values[0]] + abc_values[1]
    return registers

def mulr(registers, abc_values):
    registers[abc_values[2]] = registers[abc_values[0]] * registers[abc_values[1]]
    return registers

def muli(registers, abc_values):
    registers[abc_values[2]] = registers[abc_values[0]] * abc_values[1]
    return registers

def banr(registers, abc_values):
    registers[abc_values[2]] = registers[abc_values[0]] & registers[abc_values[1]]
    return registers

def bani(registers, abc_values):
    registers[abc_values[2]] = registers[abc_values[0]] & abc_values[1]
    return registers

def borr(registers, abc_values):
    registers[abc_values[2]] = registers[abc_values[0]] | registers[abc_values[1]]
    return registers

def bori(registers, abc_values):
    registers[abc_values[2]] = registers[abc_values[0]] | abc_values[1]
    return registers

def setr(registers, abc_values):
    registers[abc_values[2]] = registers[abc_values[0]]
    return registers

def seti(registers, abc_values):
    registers[abc_values[2]] = abc_values[0]
    return registers

def gtir(registers, abc_values):
    registers[abc_values[2]] = 1 if abc_values[0] > registers[abc_values[1]] else 0
    return registers

def gtri(registers, abc_values):
    registers[abc_values[2]] = 1 if registers[abc_values[0]] > abc_values[1] else 0
    return registers

def gtrr(registers, abc_values):
    registers[abc_values[2]] = 1 if registers[abc_values[0]] > registers[abc_values[1]] else 0
    return registers

def eqir(registers, abc_values):
    registers[abc_values[2]] = 1 if abc_values[0] == registers[abc_values[1]] else 0
    return registers

def eqri(registers, abc_values):
    registers[abc_values[2]] = 1 if registers[abc_values[0]] == abc_values[1] else 0
    return registers

def eqrr(registers, abc_values):
    registers[abc_values[2]] = 1 if registers[abc_values[0]] == registers[abc_values[1]] else 0
    return registers

opcode_names_to_funcs = {
    "addr": addr, "addi": addi,
    "mulr": mulr, "muli": muli,
    "banr": banr, "bani": bani,
    "borr": borr, "bori": bori,
    "setr": setr, "seti": seti,
    "gtir": gtir, "gtri": gtri, "gtrr": gtrr,
    "eqir": eqir, "eqri": eqri, "eqrr": eqrr,
}

def main():
    with open("input.txt", "r") as file:
        input_data = file.read()
    print(solve(input_data))

if __name__ == "__main__":
    main()
