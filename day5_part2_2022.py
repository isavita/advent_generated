
def move(st, steps):
    stacks = [[] for _ in range(len(st))]
    for i in range(len(st)):
        for j in range(len(st[i])):
            stacks[i].append(st[i][len(st[i])-j-1])

    for step in steps:
        n, frm, to = map(int, step.split()[1::2])
        frm -= 1
        to -= 1
        stacks[to] += stacks[frm][-n:]
        stacks[frm] = stacks[frm][:-n]

    b = [stacks[i][-1] for i in range(len(stacks))]
    return "".join(b)


with open("input.txt", "r") as file:
    data = file.read().split("\n\n")
    input_data = data[0].split("\n")
    stacks = [[] for _ in range((len(input_data[0])+1)//4)]
    for line in input_data:
        for i, b in enumerate(line):
            if ord(b) >= ord('A') and ord(b) <= ord('Z'):
                stacks[(i-1)//4].append(b)

    steps = data[1].split("\n")
    print(move(stacks, steps))
