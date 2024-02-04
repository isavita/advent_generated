
def run_intcode(noun, verb, program):
    program[1] = noun
    program[2] = verb
    i = 0
    while program[i] != 99:
        op = program[i]
        if op == 1:
            program[program[i + 3]] = program[program[i + 1]] + program[program[i + 2]]
        elif op == 2:
            program[program[i + 3]] = program[program[i + 1]] * program[program[i + 2]]
        i += 4
    return program[0]

with open('input.txt', 'r') as file:
    data = file.read().strip()
    program = list(map(int, data.split(',')))

# Part One
print(run_intcode(12, 2, program.copy()))

# Part Two
for noun in range(100):
    for verb in range(100):
        if run_intcode(noun, verb, program.copy()) == 19690720:
            print(100 * noun + verb)
            break
