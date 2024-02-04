with open('input.txt') as f:
    data = f.read().strip().split('\n')
    genA_start = int(data[0].split()[-1])
    genB_start = int(data[1].split()[-1])

def generator(prev, factor, criteria):
    while True:
        prev = (prev * factor) % 2147483647
        if prev % criteria == 0:
            yield prev & 0xFFFF

genA = generator(genA_start, 16807, 4)
genB = generator(genB_start, 48271, 8)

total = 0
for _ in range(5000000):
    if next(genA) == next(genB):
        total += 1

print(total)