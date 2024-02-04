from itertools import combinations

def count_combinations(containers, target):
    count = 0
    for i in range(1, len(containers)+1):
        for combo in combinations(containers, i):
            if sum(combo) == target:
                count += 1
    return count

with open('input.txt', 'r') as file:
    containers = [int(line.strip()) for line in file]

target = 150
print(count_combinations(containers, target))