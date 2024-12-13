
def solve():
    offset = 10000000000000
    machines = read_input("input.txt")
    results = []
    for ax, ay, bx, by, px, py in machines:
        cost = solve_machine(ax, ay, bx, by, px + offset, py + offset)
        if cost >= 0:
            results.append(cost)
    if not results:
        print("0 0")
        return
    print(len(results), sum(results))

def read_input(filename):
    machines = []
    with open(filename, 'r') as f:
        lines = []
        for line in f:
            line = line.strip()
            if not line:
                if lines:
                    machines.append(parse_machine(lines))
                    lines = []
            else:
                lines.append(line)
        if lines:
            machines.append(parse_machine(lines))
    return machines

def parse_machine(lines):
    ax, ay, bx, by, px, py = 0, 0, 0, 0, 0, 0
    for line in lines:
        line = line.replace("Button A:", "A:").replace("Button B:", "B:").replace("Prize:", "P:")
        if line.startswith("A:"):
            ax, ay = parse_line(line[2:])
        elif line.startswith("B:"):
            bx, by = parse_line(line[2:])
        elif line.startswith("P:"):
            px, py = parse_prize(line[2:])
    return ax, ay, bx, by, px, py

def parse_line(s):
    parts = s.strip().split(",")
    return int(parts[0].replace("X+", "").replace("Y+", "").replace("X=", "").replace("Y=", "").strip()), int(parts[1].replace("X+", "").replace("Y+", "").replace("X=", "").replace("Y=", "").strip())

def parse_prize(s):
    parts = s.strip().split(",")
    return int(parts[0].replace("X=", "").strip()), int(parts[1].replace("Y=", "").strip())

def solve_machine(ax, ay, bx, by, px, py):
    D = ax * by - ay * bx
    if D == 0:
        return -1
    numA = px * by - py * bx
    numB = -px * ay + py * ax
    if numA % D != 0 or numB % D != 0:
        return -1
    a = numA // D
    b = numB // D
    if a < 0 or b < 0:
        return -1
    return 3 * a + b

solve()
