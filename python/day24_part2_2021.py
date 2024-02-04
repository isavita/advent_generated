
import os

def read_all(path):
    with open(path, 'r') as file:
        return file.read().strip()

def num(w):
    n = 0
    for i in range(len(w)):
        n *= 10
        n += w[i]
    return n

def main():
    k, l, m = [], [], []
    for i, line in enumerate(read_all("input.txt").split("\n")):
        v = 0
        if i % 18 == 4:
            v = int(line.split()[2])
            l.append(v)
        elif i % 18 == 5:
            v = int(line.split()[2])
            k.append(v)
        elif i % 18 == 15:
            v = int(line.split()[2])
            m.append(v)

    constraints = {}
    stack = []
    for i in range(len(l)):
        if l[i] == 1:
            stack.append(i)
        elif l[i] == 26:
            pop = stack.pop()
            constraints[pop] = [i, m[pop] + k[i]]

    min_val = [0] * 14
    for i in range(14):
        if i not in constraints:
            continue
        vmin = 1
        while vmin + constraints[i][1] < 1:
            vmin += 1
        min_val[i] = vmin
        min_val[constraints[i][0]] = vmin + constraints[i][1]

    print(num(min_val))

if __name__ == "__main__":
    main()
