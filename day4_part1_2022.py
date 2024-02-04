
with open('input.txt', 'r') as file:
    data = file.read().splitlines()

def is_fully_contained(range1, range2):
    start1, end1 = map(int, range1.split('-'))
    start2, end2 = map(int, range2.split('-'))
    return start1 <= start2 and end1 >= end2

count = 0
for line in data:
    range1, range2 = line.split(',')
    if is_fully_contained(range1, range2) or is_fully_contained(range2, range1):
        count += 1

print(count)
