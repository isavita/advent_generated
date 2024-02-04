
from collections import defaultdict

claims = []
fabric = defaultdict(int)

with open('input.txt') as f:
    for line in f:
        claim_id, _, coordinates, dimensions = line.split()
        x, y = map(int, coordinates[:-1].split(','))
        width, height = map(int, dimensions.split('x'))
        claims.append((claim_id, x, y, width, height))
        for i in range(x, x + width):
            for j in range(y, y + height):
                fabric[(i, j)] += 1

overlapping_inches = sum(1 for value in fabric.values() if value > 1)
print(overlapping_inches)

for claim_id, x, y, width, height in claims:
    if all(fabric[(i, j)] == 1 for i in range(x, x + width) for j in range(y, y + height)):
        print(claim_id)
        break
