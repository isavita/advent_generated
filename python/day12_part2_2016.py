
with open('input.txt', 'r') as file:
    instructions = [line.strip() for line in file]

registers = {"a": 0, "b": 0, "c": 1, "d": 0}

def executeInstructions(instructions, registers):
    i = 0
    while i < len(instructions):
        parts = instructions[i].split()
        if parts[0] == "cpy":
            val = registers[parts[1]] if parts[1].isalpha() else int(parts[1])
            registers[parts[2]] = val
            i += 1
        elif parts[0] == "inc":
            registers[parts[1]] += 1
            i += 1
        elif parts[0] == "dec":
            registers[parts[1]] -= 1
            i += 1
        elif parts[0] == "jnz":
            val = registers[parts[1]] if parts[1].isalpha() else int(parts[1])
            jump = int(parts[2])
            i += jump if val != 0 else 1

executeInstructions(instructions, registers)

print(registers["a"])
