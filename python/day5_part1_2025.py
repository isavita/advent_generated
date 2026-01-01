import bisect

def contains(ranges, x):
    lo, hi = 0, len(ranges)
    while lo < hi:
        mid = (lo + hi) // 2
        a, b = ranges[mid]
        if x < a:
            hi = mid
        elif x > b:
            lo = mid + 1
        else:
            return True
    return False

def main():
    with open('input.txt') as f:
        lines = [l.rstrip('\n') for l in f]
    raw = []
    ids = []
    parsing = True
    for line in lines:
        s = line.strip()
        if s == '':
            parsing = False
            continue
        if parsing:
            a, b = map(int, s.split('-'))
            raw.append((a, b))
        else:
            ids.append(int(s))
    if raw:
        raw.sort()
        merged = []
        for a, b in raw:
            if not merged or a > merged[-1][1]:
                merged.append([a, b])
            elif b > merged[-1][1]:
                merged[-1][1] = b
        merged = [tuple(p) for p in merged]
    else:
        merged = []
    fresh = sum(1 for v in ids if contains(merged, v))
    print(f'Number of fresh ingredients: {fresh}')

if __name__ == '__main__':
    main()
