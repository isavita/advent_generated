def execute_program(instructions):
    registers = {'a': 0, 'b': 0}
    index = 0

    def hlf(r):
        registers[r] //= 2
        return index + 1

    def tpl(r):
        registers[r] *= 3
        return index + 1

    def inc(r):
        registers[r] += 1
        return index + 1

    def jmp(offset):
        return index + int(offset)

    def jie(r, offset):
        return index + int(offset) if registers[r] % 2 == 0 else index + 1

    def jio(r, offset):
        return index + int(offset) if registers[r] == 1 else index + 1

    while 0 <= index < len(instructions):
        instruction = instructions[index].split()
        op = instruction[0]
        if op == 'hlf':
            index = hlf(instruction[1])
        elif op == 'tpl':
            index = tpl(instruction[1])
        elif op == 'inc':
            index = inc(instruction[1])
        elif op == 'jmp':
            index = jmp(instruction[1])
        elif op == 'jie':
            index = jie(instruction[1][0], instruction[2])
        elif op == 'jio':
            index = jio(instruction[1][0], instruction[2])

    return registers['b']

# Read instructions from file
with open('input.txt', 'r') as file:
    instructions = file.read().splitlines()

# Execute program and print result
result = execute_program(instructions)
print(result)
