
with open("input.txt", "r") as file:
    instructions = [line.strip().split() for line in file]

def get_value(arg, registers):
    try:
        return int(arg)
    except ValueError:
        return registers[arg]

registers0 = {"p": 0}
registers1 = {"p": 1}
queue0 = []
queue1 = []
send_count1 = 0
i0, i1 = 0, 0
deadlock0, deadlock1 = False, False

while not (deadlock0 and deadlock1):
    deadlock0, deadlock1 = True, True

    # Program 0
    while i0 < len(instructions):
        instruction = instructions[i0]
        cmd = instruction[0]
        arg1 = instruction[1]
        if cmd == "snd":
            queue1.append(get_value(arg1, registers0))
        elif cmd == "set":
            registers0[arg1] = get_value(instruction[2], registers0)
        elif cmd == "add":
            registers0[arg1] += get_value(instruction[2], registers0)
        elif cmd == "mul":
            registers0[arg1] *= get_value(instruction[2], registers0)
        elif cmd == "mod":
            registers0[arg1] %= get_value(instruction[2], registers0)
        elif cmd == "rcv":
            if len(queue0) == 0:
                break
            registers0[arg1] = queue0.pop(0)
        elif cmd == "jgz":
            if get_value(arg1, registers0) > 0:
                i0 += get_value(instruction[2], registers0)
                continue
        i0 += 1
        deadlock0 = False

    # Program 1
    while i1 < len(instructions):
        instruction = instructions[i1]
        cmd = instruction[0]
        arg1 = instruction[1]
        if cmd == "snd":
            queue0.append(get_value(arg1, registers1))
            send_count1 += 1
        elif cmd == "set":
            registers1[arg1] = get_value(instruction[2], registers1)
        elif cmd == "add":
            registers1[arg1] += get_value(instruction[2], registers1)
        elif cmd == "mul":
            registers1[arg1] *= get_value(instruction[2], registers1)
        elif cmd == "mod":
            registers1[arg1] %= get_value(instruction[2], registers1)
        elif cmd == "rcv":
            if len(queue1) == 0:
                break
            registers1[arg1] = queue1.pop(0)
        elif cmd == "jgz":
            if get_value(arg1, registers1) > 0:
                i1 += get_value(instruction[2], registers1)
                continue
        i1 += 1
        deadlock1 = False

print(send_count1)
