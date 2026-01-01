import re

def is_invalid(x):
    s = str(x)
    n = len(s)
    if n <= 1:
        return False
    for p in range(1, n // 2 + 1):
        if n % p:
            continue
        if s[:p] * (n // p) == s:
            return True
    return False

def main():
    txt = open("input.txt").read()
    total = 0
    for a_str, b_str in re.findall(r"(\d+)-(\d+)", txt):
        a = int(a_str)
        b = int(b_str)
        lo, hi = (a, b) if a <= b else (b, a)
        x = lo
        while True:
            if is_invalid(x):
                total += x
            if x == hi:
                break
            x += 1
    print(total)

if __name__ == "__main__":
    main()
