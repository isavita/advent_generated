
with open("input.txt", "r") as file:
    firewall = {}
    for line in file:
        depth, rng = map(int, line.strip().split(": "))
        firewall[depth] = {"Range": rng, "Position": 0, "Direction": 1}

severity = 0

for depth in range(max(firewall.keys()) + 1):
    if depth in firewall:
        if firewall[depth]["Position"] == 0:
            severity += depth * firewall[depth]["Range"]

    for scanner in firewall.values():
        if scanner["Position"] == 0:
            scanner["Direction"] = 1
        elif scanner["Position"] == scanner["Range"] - 1:
            scanner["Direction"] = -1
        scanner["Position"] += scanner["Direction"]

print(severity)
