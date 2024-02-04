
f = open("input.txt", "r")
_ = int(f.readline())
buses = [(int(x), i) for i, x in enumerate(f.readline().split(',')) if x != 'x']

t, step = 1, 1
for bus, offset in buses:
    while (t + offset) % bus != 0:
        t += step
    step *= bus

print(t)
