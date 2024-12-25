
def solve():
    with open("input.txt", "r") as f:
        raw = [line.strip() for line in f if line.strip()]
    if len(raw) % 7 != 0:
        print(0)
        return
    locks, keys = [], []
    for i in range(0, len(raw), 7):
        block = raw[i:i + 7]
        if any(len(ln) < 5 for ln in block):
            continue
        if all(c == '#' for c in block[0]):
            locks.append(parse_lock(block))
        else:
            keys.append(parse_key(block))
    count = 0
    for lock in locks:
        for key in keys:
            if fits(lock, key):
                count += 1
    print(count)

def parse_lock(b):
    h = []
    for c in range(5):
        cnt = 0
        for r in range(1, 7):
            if b[r][c] == '#':
                cnt += 1
            else:
                break
        h.append(cnt)
    return h

def parse_key(b):
    h = []
    for c in range(5):
        cnt = 0
        for r in range(5, -1, -1):
            if b[r][c] == '#':
                cnt += 1
            else:
                break
        h.append(cnt)
    return h

def fits(lock, key):
    return all(lock[i] + key[i] <= 5 for i in range(5))

if __name__ == "__main__":
    solve()
