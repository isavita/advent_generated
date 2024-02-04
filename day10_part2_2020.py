
from collections import Counter

with open("input.txt") as f:
    adapters = [0] + sorted([int(line) for line in f.readlines()])
    adapters.append(adapters[-1] + 3)

diffs = [adapters[i + 1] - adapters[i] for i in range(len(adapters) - 1)]
diff_counts = Counter(diffs)

print(diff_counts[1] * diff_counts[3])

arrangements = [1] + [0] * (len(adapters) - 1)

for i in range(1, len(adapters)):
    for j in range(i):
        if adapters[i] - adapters[j] <= 3:
            arrangements[i] += arrangements[j]

print(arrangements[-1])
