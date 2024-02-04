
import os

def read_all(path):
    with open(path, 'r') as file:
        return file.read()

def num(w):
    n = 0
    for i in w:
        n *= 10
        n += i
    return n

def main():
    k, l, m = [], [], []
    lines = read_all("input.txt").split("\n")
    for i, line in enumerate(lines):
        v = 0
        if i % 18 == 4:
            v = int(line.split()[-1])
            l.append(v)
        elif i % 18 == 5:
            v = int(line.split()[-1])
            k.append(v)
        elif i % 18 == 15:
            v = int(line.split()[-1])
            m.append(v)

    constraints = {}
    stack = []
    for i in range(len(l)):
        if l[i] == 1:
            stack.append(i)
        elif l[i] == 26:
            pop = stack.pop()
            constraints[pop] = [i, m[pop] + k[i]]

    max_vals = [0] * 14
    for i in range(14):
        if i not in constraints:
            continue
        vmax = 9
        while vmax + constraints[i][1] > 9:
            vmax -= 1
        max_vals[i] = vmax
        max_vals[constraints[i][0]] = vmax + constraints[i][1]

    print(num(max_vals))

if __name__ == "__main__":
    main()
