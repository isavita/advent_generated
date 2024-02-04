
from collections import Counter

with open('input.txt', 'r') as file:
    box_ids = [line.strip() for line in file]

num_twos = 0
num_threes = 0

for box_id in box_ids:
    counts = Counter(box_id)
    if 2 in counts.values():
        num_twos += 1
    if 3 in counts.values():
        num_threes += 1

print(num_twos * num_threes)
