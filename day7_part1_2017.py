with open('input.txt', 'r') as file:
    data = file.read().splitlines()

programs = []
held_by = []

for line in data:
    if '->' in line:
        program, weight_and_above = line.split(' -> ')
        programs.append(program.split()[0])
        above = weight_and_above.split(', ')
        for program_above in above:
            held_by.append(program_above)
    else:
        program = line.split()[0]
        programs.append(program)

bottom_program = set(programs) - set(held_by)
print(bottom_program.pop())