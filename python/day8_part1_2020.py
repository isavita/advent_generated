accumulator = 0
index = 0
visited = set()

instructions = []
with open('input.txt', 'r') as file:
    for line in file:
        instructions.append(line.strip())

while index not in visited:
    visited.add(index)
    instruction, argument = instructions[index].split()
    argument = int(argument)

    if instruction == 'acc':
        accumulator += argument
        index += 1
    elif instruction == 'jmp':
        index += argument
    elif instruction == 'nop':
        index += 1

print(accumulator)