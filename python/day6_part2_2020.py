
from collections import Counter

with open('input.txt', 'r') as file:
    groups = file.read().strip().split('\n\n')

# Part One
total_count = 0
for group in groups:
    group = group.replace('\n', '')
    total_count += len(set(group))

print(total_count)

# Part Two
total_count = 0
for group in groups:
    group_list = group.split('\n')
    num_people = len(group_list)
    group_str = ''.join(group_list)
    counter = Counter(group_str)
    for count in counter.values():
        if count == num_people:
            total_count += 1

print(total_count)
