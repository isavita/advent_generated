
from collections import Counter

with open('input.txt', 'r') as file:
    data = file.read().splitlines()

def calculate_priority(s):
    return ord(s) - ord('a') + 1 if s.islower() else ord(s) - ord('A') + 27

def part_one():
    total = 0
    for line in data:
        first_half = line[:len(line)//2]
        second_half = line[len(line)//2:]
        common_items = set(first_half) & set(second_half)
        for item in common_items:
            total += calculate_priority(item)
    print(total)

def part_two():
    total = 0
    for i in range(0, len(data), 3):
        group = data[i:i+3]
        common_items = set(group[0]) & set(group[1]) & set(group[2])
        for item in common_items:
            total += calculate_priority(item)
    print(total)

part_one()
part_two()
