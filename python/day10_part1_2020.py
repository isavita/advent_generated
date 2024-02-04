
from collections import Counter

with open('input.txt') as f:
    adapters = [int(x) for x in f.read().strip().split('\n')]

adapters.sort()
adapters.insert(0, 0)
adapters.append(adapters[-1] + 3)

diffs = [adapters[i+1] - adapters[i] for i in range(len(adapters)-1)]
count_diffs = Counter(diffs)

print(count_diffs[1] * count_diffs[3])
