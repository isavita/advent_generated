with open("input.txt") as f:
    data = f.readlines()

steps = set()
dependencies = {}
for line in data:
    step = line[5]
    next_step = line[36]
    steps.add(step)
    steps.add(next_step)
    if next_step not in dependencies:
        dependencies[next_step] = set()
    dependencies[next_step].add(step)

order = []
while steps:
    available = [step for step in steps if step not in dependencies or all(dep in order for dep in dependencies[step])]
    next_step = min(available)
    order.append(next_step)
    steps.remove(next_step)

print(''.join(order))