with open("input.txt", "r") as file:
    instructions = [line.strip() for line in file]

registers = {'a': 0, 'b': 0}
current_instruction = 0

while current_instruction < len(instructions):
    instruction = instructions[current_instruction].split()

    if instruction[0] == 'hlf':
        registers[instruction[1]] //= 2
        current_instruction += 1
    elif instruction[0] == 'tpl':
        registers[instruction[1]] *= 3
        current_instruction += 1
    elif instruction[0] == 'inc':
        registers[instruction[1]] += 1
        current_instruction += 1
    elif instruction[0] == 'jmp':
        current_instruction += int(instruction[1])
    elif instruction[0] == 'jie':
        if registers[instruction[1][0]] % 2 == 0:
            current_instruction += int(instruction[2])
        else:
            current_instruction += 1
    elif instruction[0] == 'jio':
        if registers[instruction[1][0]] == 1:
            current_instruction += int(instruction[2])
        else:
            current_instruction += 1

print(registers['b'])