
from itertools import combinations

containers = []
total_liters = 150

with open('input.txt', 'r') as file:
    for line in file:
        containers.append(int(line.strip()))

count = 0
min_containers = float('inf')
for i in range(1, len(containers)+1):
    for combo in combinations(containers, i):
        if sum(combo) == total_liters:
            count += 1
            if i < min_containers:
                min_containers = i

print(count)
print(sum(1 for combo in combinations(containers, min_containers) if sum(combo) == total_liters))
