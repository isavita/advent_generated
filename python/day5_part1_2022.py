
def move(st, steps):
    stacks = [[] for _ in range(len(st))]
    for i in range(len(st)):
        for j in range(len(st[i])):
            stacks[i].append(st[i][len(st[i])-j-1])

    for step in steps:
        n, from_, to = map(int, step.split()[1::2])
        from_ -= 1
        to -= 1
        for _ in range(n):
            stacks[to].append(stacks[from_][-1])
            stacks[from_] = stacks[from_][:-1]

    b = [stacks[i][-1] for i in range(len(stacks))]
    return "".join(b)

with open("input.txt", "r") as file:
    data = file.read().split("\n\n")
    input_ = data[0].split("\n")
    stacks = [[] for _ in range((len(input_[0])+1)//4)]
    for line in input_:
        for i, b in enumerate(line):
            if 'A' <= b <= 'Z':
                stacks[(i-1)//4].append(b)

    steps = data[1].split("\n")
    print(move(stacks, steps))
