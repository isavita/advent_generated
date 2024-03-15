import json
from functools import cmp_to_key

def read_all(path):
    with open(path, 'r') as file:
        return file.read()

def compare(a, b):
    if isinstance(a, int) and isinstance(b, int):
        return -1 if a < b else 1 if a > b else 0
    if isinstance(a, int):
        return compare([a], b)
    if isinstance(b, int):
        return compare(a, [b])
    for i in range(min(len(a), len(b))):
        if (c := compare(a[i], b[i])) != 0:
            return c
    return -1 if len(a) < len(b) else 1 if len(a) > len(b) else 0

def main():
    s = read_all('input.txt')
    packets = []
    for pair in s.split('\n\n'):
        first, second = (json.loads(line) for line in pair.split('\n'))
        packets.append(first)
        packets.append(second)

    divider1, divider2 = json.loads('[[2]]'), json.loads('[[6]]')
    packets.append(divider1)
    packets.append(divider2)
    packets.sort(key=cmp_to_key(compare))

    divider1_pos = packets.index(divider1) + 1
    divider2_pos = packets.index(divider2) + 1
    print(divider1_pos * divider2_pos)

if __name__ == '__main__':
    main()