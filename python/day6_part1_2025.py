import sys

def is_separator(col, lines):
    for ln in lines:
        if col < len(ln) and not ln[col].isspace():
            return False
    return True

def block_result(sc, ec, lines):
    numbers = []
    op = 0
    for ln in lines:
        end = min(ec + 1, len(ln))
        if sc >= len(ln):
            continue
        seg = ln[sc:end].strip()
        if not seg:
            continue
        if seg == '+':
            op = 1
        elif seg == '*':
            op = 2
        else:
            numbers.append(int(seg))
    if not numbers:
        return 0
    if op == 1:
        s = 0
        for v in numbers:
            s += v
        return s
    if op == 2:
        p = 1
        for v in numbers:
            p *= v
        return p
    return numbers[0]

def main():
    lines = open("input.txt").read().splitlines()
    max_w = max((len(l) for l in lines), default=0)
    grand = 0
    in_block = False
    start = 0
    for x in range(max_w):
        if not is_separator(x, lines):
            if not in_block:
                in_block = True
                start = x
        else:
            if in_block:
                grand += block_result(start, x - 1, lines)
                in_block = False
    if in_block:
        grand += block_result(start, max_w - 1, lines)
    print(f"Grand total: {grand}")

if __name__ == "__main__":
    main()
