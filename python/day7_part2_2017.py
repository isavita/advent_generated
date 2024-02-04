
import re

def dfs(name, programs):
    program = programs[name]
    total_weight = program['weight']

    weights = {}
    for child in program['holds']:
        weight, balanced = dfs(child, programs)
        if not balanced:
            return 0, False
        total_weight += weight
        if weight in weights:
            weights[weight] += 1
        else:
            weights[weight] = 1

    for w1, c1 in weights.items():
        for w2, c2 in weights.items():
            if w1 != w2 and c1 < c2:
                for child in program['holds']:
                    if dfs(child, programs)[0] == w1:
                        unbalanced_program = child
                        break
                print(programs[unbalanced_program]['weight'] + (w2 - w1))
                return 0, False
    return total_weight, True

# Read Input
with open("input.txt", "r") as file:
    lines = file.read().splitlines()

# Create Data Structure
programs = {}

for line in lines:
    matches = re.findall(r'[a-z]+|\d+', line)
    name = matches[0]
    weight = int(matches[1])

    program = {'weight': weight}
    if len(matches) > 2:
        program['holds'] = matches[2:]
    else:
        program['holds'] = []
    programs[name] = program

# Find Root (from Part One)
root = "dtacyn"  # Replace this with the root found in Part One

# Identify Unbalance
dfs(root, programs)
