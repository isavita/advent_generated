nanobots = []
with open("input.txt", "r") as file:
    for line in file:
        pos = tuple(map(int, line.split("pos=<")[1].split(">")[0].split(",")))
        r = int(line.split("r=")[1].strip())
        nanobots.append((pos, r))

strongest_nanobot = max(nanobots, key=lambda x: x[1])
count = sum(1 for n in nanobots if sum(abs(n[0][i] - strongest_nanobot[0][i]) for i in range(3)) <= strongest_nanobot[1])

print(count)