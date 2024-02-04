
x = [1]
with open("input.txt", "r") as f:
    lines = f.read().split("\n")
    for line in lines:
        if line == "noop":
            x.append(x[-1])
        else:
            n = int(line.split()[1])
            x.append(x[-1])
            x.append(x[-1] + n)

sum = 0
for i in range(len(x)):
    if (i - 19) % 40 == 0:
        sum += (i + 1) * x[i]

print(sum)
