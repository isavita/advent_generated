with open('input.txt', 'r') as file:
    data = file.readlines()

pipes = {}
for line in data:
    line = line.strip().split(' ')
    program = int(line[0])
    connected_to = [int(x.strip(',')) for x in line[2:]]
    pipes[program] = connected_to

def find_group(program, group):
    group.add(program)
    for p in pipes[program]:
        if p not in group:
            find_group(p, group)

group_0 = set()
find_group(0, group_0)

print(len(group_0))