
with open("input.txt", "r") as file:
    instructions = file.read().strip().split("\n")

registers = {"a": 1, "b": 0}

i = 0
while i < len(instructions):
    parts = instructions[i].split()

    if parts[0] == "hlf":
        registers[parts[1]] //= 2
    elif parts[0] == "tpl":
        registers[parts[1]] *= 3
    elif parts[0] == "inc":
        registers[parts[1]] += 1
    elif parts[0] == "jmp":
        offset = int(parts[1])
        i += offset - 1
    elif parts[0] == "jie":
        if registers[parts[1][0]] % 2 == 0:
            offset = int(parts[2])
            i += offset - 1
    elif parts[0] == "jio":
        if registers[parts[1][0]] == 1:
            offset = int(parts[2])
            i += offset - 1
    else:
        print("Unknown instruction:", parts[0])
        break

    i += 1

print(registers["b"])
