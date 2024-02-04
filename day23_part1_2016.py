
import sys

def read_instructions(filename):
    with open(filename, 'r') as file:
        return file.read().splitlines()

def execute_instructions(instructions, registers):
    pc = 0
    while pc < len(instructions):
        fields = instructions[pc].split()
        if fields[0] == "cpy":
            x = int(registers.get(fields[1], fields[1]))
            if fields[2] in registers:
                registers[fields[2]] = x
        elif fields[0] == "inc":
            if fields[1] in registers:
                registers[fields[1]] += 1
        elif fields[0] == "dec":
            if fields[1] in registers:
                registers[fields[1]] -= 1
        elif fields[0] == "jnz":
            x = int(registers.get(fields[1], fields[1]))
            if x != 0:
                pc += int(registers.get(fields[2], fields[2])) - 1
        elif fields[0] == "tgl":
            x = int(registers.get(fields[1], fields[1]))
            tgt = pc + x
            if tgt >= 0 and tgt < len(instructions):
                instructions[tgt] = toggle_instruction(instructions[tgt])
        pc += 1

def toggle_instruction(instr):
    parts = instr.split()
    if parts[0] == "inc":
        parts[0] = "dec"
    elif parts[0] == "dec" or parts[0] == "tgl":
        parts[0] = "inc"
    elif parts[0] == "jnz":
        parts[0] = "cpy"
    elif parts[0] == "cpy":
        parts[0] = "jnz"
    return ' '.join(parts)

instructions = read_instructions("input.txt")
registers = {"a": 7, "b": 0, "c": 0, "d": 0}
execute_instructions(instructions, registers)
print(registers["a"])
