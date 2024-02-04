
from collections import Counter

def part_one(box_ids):
    count_two = 0
    count_three = 0

    for box_id in box_ids:
        letter_count = Counter(box_id)
        if 2 in letter_count.values():
            count_two += 1
        if 3 in letter_count.values():
            count_three += 1

    return count_two * count_three

def part_two(box_ids):
    for i in range(len(box_ids)):
        for j in range(i + 1, len(box_ids)):
            diff_count = sum(1 for x, y in zip(box_ids[i], box_ids[j]) if x != y)
            if diff_count == 1:
                return ''.join(x for x, y in zip(box_ids[i], box_ids[j]) if x == y)

if __name__ == '__main__':
    with open('input.txt', 'r') as file:
        box_ids = [line.strip() for line in file]

    print(part_one(box_ids))
    print(part_two(box_ids))
