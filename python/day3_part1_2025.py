import sys

def calc(s: str) -> int:
    n = len(s)
    suf = [-1] * (n + 1)
    for i in range(n - 1, -1, -1):
        c = s[i]
        if '0' <= c <= '9':
            d = ord(c) - 48
            suf[i] = d if d > suf[i + 1] else suf[i + 1]
        else:
            suf[i] = suf[i + 1]
    best = 0
    for i in range(n - 1):
        c = s[i]
        if '0' <= c <= '9' and suf[i + 1] != -1:
            d1 = ord(c) - 48
            val = d1 * 10 + suf[i + 1]
            if val > best:
                best = val
    return best

def main():
    total = 0
    with open("input.txt") as f:
        for line in f:
            total += calc(line.rstrip('\n'))
    sys.stdout.write(str(total))

if __name__ == "__main__":
    main()
