
with open('input.txt') as f:
    num_elves = int(f.read())

def josephus(n):
    i = 1
    while i * 3 <= n:
        i *= 3
    return n - i + max(n - 2 * i, 0)

print(josephus(num_elves))
