
import json

def read_all(path):
    with open(path, 'r') as file:
        return file.read()

def compare(a, b):
    anum = isinstance(a, (int, float))
    bnum = isinstance(b, (int, float))
    if anum and bnum:
        return sign(int(a) - int(b))
    elif anum:
        return compare([a], b)
    elif bnum:
        return compare(a, [b])
    else:
        aa, bb = a, b
        for i in range(min(len(aa), len(bb))):
            c = compare(aa[i], bb[i])
            if c != 0:
                return c
        return sign(len(aa) - len(bb))

def sign(n):
    if n == 0:
        return 0
    elif n < 0:
        return -1
    else:
        return 1

def main():
    s = read_all("input.txt")
    packets = []
    sum = 0
    for i, pair in enumerate(s.strip().split("\n\n")):
        sp = pair.split("\n")
        first, second = json.loads(sp[0]), json.loads(sp[1])
        packets.extend([first, second])
        if compare(first, second) == -1:
            sum += i + 1
    print(sum)

if __name__ == "__main__":
    main()
