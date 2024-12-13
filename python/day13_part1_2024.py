
import re

def parse_val(s):
    s = s.strip()
    s = re.sub(r"^(X\+|Y\+|X=|Y=)", "", s)
    return int(s)

def parse_val_prize(s):
    s = s.strip()
    s = re.sub(r"^(X=|Y=)", "", s)
    return int(s)

def parse_line(s):
    parts = s.strip().split(',')
    x = parse_val(parts[0])
    y = parse_val(parts[1])
    return x, y

def parse_prize(s):
    parts = s.strip().split(',')
    x = parse_val_prize(parts[0])
    y = parse_val_prize(parts[1])
    return x, y

def parse_machine(lines):
    m = {}
    for l in lines:
        l = l.replace("Button A:", "A:")
        l = l.replace("Button B:", "B:")
        l = l.replace("Prize:", "P:")
        if l.startswith("A:"):
            m['ax'], m['ay'] = parse_line(l[2:])
        elif l.startswith("B:"):
            m['bx'], m['by'] = parse_line(l[2:])
        elif l.startswith("P:"):
            m['px'], m['py'] = parse_prize(l[2:])
    return m

def solve_machine(m):
    min_cost = -1
    for a_count in range(101):
        for b_count in range(101):
            x = m['ax'] * a_count + m['bx'] * b_count
            y = m['ay'] * a_count + m['by'] * b_count
            if x == m['px'] and y == m['py']:
                cost = a_count * 3 + b_count
                if min_cost == -1 or cost < min_cost:
                    min_cost = cost
    return min_cost

def main():
    with open("input.txt", "r") as f:
        lines = f.readlines()

    machines = []
    current_machine = []
    for line in lines:
        line = line.strip()
        if not line:
            if current_machine:
                machines.append(parse_machine(current_machine))
                current_machine = []
        else:
            current_machine.append(line)
    if current_machine:
        machines.append(parse_machine(current_machine))

    results = [solve_machine(m) for m in machines if solve_machine(m) != -1]
    if not results:
        print("0 0")
    else:
        print(len(results), sum(results))

if __name__ == "__main__":
    main()

