from collections import defaultdict

starting_numbers = [int(x) for x in open("input.txt").read().split(",")]

last_seen = defaultdict(int)
for i, num in enumerate(starting_numbers[:-1]):
    last_seen[num] = i + 1

last_num = starting_numbers[-1]
for i in range(len(starting_numbers), 2020):
    if last_num in last_seen:
        new_num = i - last_seen[last_num]
    else:
        new_num = 0
    last_seen[last_num] = i
    last_num = new_num

print(last_num)