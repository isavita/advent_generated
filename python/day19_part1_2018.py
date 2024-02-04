
with open('input.txt', 'r') as file:
    data = file.read().strip().split('\n')

ip_register = int(data[0].split()[1])
instructions = data[1:]

registers = [0, 0, 0, 0, 0, 0]

while 0 <= registers[ip_register] < len(instructions):
    instruction_pointer = registers[ip_register]
    instruction = instructions[instruction_pointer].split()
    opcode, A, B, C = instruction[0], int(instruction[1]), int(instruction[2]), int(instruction[3])

    if opcode == 'addr':
        registers[C] = registers[A] + registers[B]
    elif opcode == 'addi':
        registers[C] = registers[A] + B
    elif opcode == 'mulr':
        registers[C] = registers[A] * registers[B]
    elif opcode == 'muli':
        registers[C] = registers[A] * B
    elif opcode == 'banr':
        registers[C] = registers[A] & registers[B]
    elif opcode == 'bani':
        registers[C] = registers[A] & B
    elif opcode == 'borr':
        registers[C] = registers[A] | registers[B]
    elif opcode == 'bori':
        registers[C] = registers[A] | B
    elif opcode == 'setr':
        registers[C] = registers[A]
    elif opcode == 'seti':
        registers[C] = A
    elif opcode == 'gtir':
        registers[C] = 1 if A > registers[B] else 0
    elif opcode == 'gtri':
        registers[C] = 1 if registers[A] > B else 0
    elif opcode == 'gtrr':
        registers[C] = 1 if registers[A] > registers[B] else 0
    elif opcode == 'eqir':
        registers[C] = 1 if A == registers[B] else 0
    elif opcode == 'eqri':
        registers[C] = 1 if registers[A] == B else 0
    elif opcode == 'eqrr':
        registers[C] = 1 if registers[A] == registers[B] else 0

    registers[ip_register] += 1

print(registers[0])
