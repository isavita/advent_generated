
from collections import defaultdict

claims = []
fabric = defaultdict(int)

with open('input.txt', 'r') as file:
    for line in file:
        parts = line.split()
        claim_id = int(parts[0][1:])
        left_offset, top_offset = map(int, parts[2][:-1].split(','))
        width, height = map(int, parts[3].split('x'))
        
        claims.append((claim_id, left_offset, top_offset, width, height))
        
        for i in range(left_offset, left_offset + width):
            for j in range(top_offset, top_offset + height):
                fabric[(i, j)] += 1

overlap_count = sum(1 for value in fabric.values() if value > 1)
print(overlap_count)
