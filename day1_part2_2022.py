
from collections import defaultdict

with open('input.txt', 'r') as file:
    data = file.read().strip().split('\n')

calories = defaultdict(int)
current_elf = 0

for line in data:
    if line == '':
        current_elf += 1
    else:
        calories[current_elf] += int(line)

sorted_calories = sorted(calories.values(), reverse=True)
top_three = sorted_calories[:3]

print(sum(top_three))
