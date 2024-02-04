with open('input.txt', 'r') as file:
    instructions = [line.strip() for line in file]

registers = {}
current_instruction = 0
last_sound = 0

while 0 <= current_instruction < len(instructions):
    instruction = instructions[current_instruction].split()
    command = instruction[0]
    register = instruction[1]

    if register not in registers:
        registers[register] = 0

    if len(instruction) == 3:
        value = instruction[2]
        if value.isalpha():
            value = registers[value]
        else:
            value = int(value)

    if command == 'snd':
        last_sound = registers[register]
    elif command == 'set':
        registers[register] = value
    elif command == 'add':
        registers[register] += value
    elif command == 'mul':
        registers[register] *= value
    elif command == 'mod':
        registers[register] %= value
    elif command == 'rcv' and registers[register] != 0:
        print(last_sound)
        break
    elif command == 'jgz' and registers[register] > 0:
        current_instruction += value
        continue

    current_instruction += 1