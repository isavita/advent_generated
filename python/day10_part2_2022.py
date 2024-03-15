from collections import defaultdict

def main():
    x = [1]
    with open("input.txt", "r") as f:
        for line in f.read().splitlines():
            if line == "noop":
                x.append(x[-1])
            else:
                n = int(line.split()[1])
                x.append(x[-1])
                x.append(x[-1] + n)

    grid = defaultdict(lambda: ".")
    for i, val in enumerate(x):
        crtx, crty = i % 40, i // 40
        if abs(crtx - val) <= 1:
            grid[(crtx, crty)] = "#"

    for y in range(6):
        print("".join(grid[(x, y)] for x in range(40)))

if __name__ == "__main__":
    main()